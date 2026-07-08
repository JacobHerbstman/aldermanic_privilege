import argparse
import math
import re
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

DIRECTION_MAP = {
    "N": "NORTH",
    "S": "SOUTH",
    "E": "EAST",
    "W": "WEST",
}
FULL_DIRECTIONS = set(DIRECTION_MAP.values())
STREET_TYPE_MAP = {
    "ST": "STREET",
    "AVE": "AVENUE",
    "AV": "AVENUE",
    "BLVD": "BOULEVARD",
    "DR": "DRIVE",
    "PL": "PLACE",
    "CT": "COURT",
    "RD": "ROAD",
    "PKY": "PARKWAY",
    "PKWY": "PARKWAY",
    "PARKWY": "PARKWAY",
    "TER": "TERRACE",
    "TERR": "TERRACE",
    "LN": "LANE",
    "HWY": "HIGHWAY",
    "CIR": "CIRCLE",
    "MKT": "MARKET",
    "MARKET": "MARKET",
}
FULL_STREET_TYPES = set(STREET_TYPE_MAP.values())
UNIT_RE = re.compile(
    r"\s+(?:#\s*[A-Z0-9-]+|APT\s+[A-Z0-9-]+|UNIT\s+[A-Z0-9-]+|STE\s+[A-Z0-9-]+|SUITE\s+[A-Z0-9-]+)\b",
    re.IGNORECASE,
)
BARE_UNIT_RE = re.compile(r"^(?:\d+[A-Z]+|[A-Z]\d+[A-Z]?|HSE|HOUSE|FLR|FL|1ST|2ND|3RD)$")
TITLE_ADDRESS_RE = re.compile(
    r"\bat\b\s+(.+?)(?:\s*(?:-\s*)?App(?:\s+No\.?)?\b|\s+-\s+[A-Z0-9-]+\s*$|\s*$)",
    re.IGNORECASE,
)
MULTI_SPLIT_RE = re.compile(r"\s+(?:AND|&)\s+|;", re.IGNORECASE)
LEADING_RANGE_RE = re.compile(r"^(\d{1,6})\s*-\s*(\d{1,6})\b")
PIN_RE = re.compile(r"\D")
POINT_RE = re.compile(r"POINT\s*\(\s*([-0-9.]+)\s+([-0-9.]+)\s*\)", re.IGNORECASE)
PAIR_RE = re.compile(r"\(?\s*([-0-9.]+)\s*,\s*([-0-9.]+)\s*\)?")
OCR_WORD_FIXES = {
    "PAUUNA": "PAULINA",
    "INDIANAPOUS": "INDIANAPOLIS",
    "WESTEM": "WESTERN",
}


def apply_ocr_word_fixes(text: str) -> str:
    out = text
    for bad, good in OCR_WORD_FIXES.items():
        out = re.sub(rf"\b{re.escape(bad)}\b", good, out, flags=re.IGNORECASE)
    return out


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-rezoning-csv", required=True)
    parser.add_argument("--in-parcel-addresses-csv", required=True)
    parser.add_argument("--in-parcel-universe-csv", required=True)
    parser.add_argument("--nearest-max-diff", type=int, default=50)
    parser.add_argument("--out-stage1-csv", required=True)
    parser.add_argument("--out-unmatched-csv", required=True)
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def choose_column(columns: list[str], options: list[str], contains: list[str] | None = None) -> str | None:
    lower_to_original = {col.lower(): col for col in columns}
    for option in options:
        if option.lower() in lower_to_original:
            return lower_to_original[option.lower()]
    if contains:
        for col in columns:
            c = col.lower()
            if all(fragment in c for fragment in contains):
                return col
    return None


def normalize_pin(value) -> str | None:
    if pd.isna(value):
        return None
    digits = PIN_RE.sub("", str(value))
    if not digits:
        return None
    if len(digits) > 14:
        digits = digits[-14:]
    return digits.zfill(14)


def safe_float(value) -> float | None:
    if pd.isna(value):
        return None
    try:
        out = float(str(value).strip())
    except Exception:  # noqa: BLE001
        return None
    if math.isnan(out) or math.isinf(out):
        return None
    return out


def parse_lat_lon_from_text(value: str | None) -> tuple[float | None, float | None]:
    if not value:
        return None, None
    text = str(value).strip()
    point = POINT_RE.search(text)
    if point:
        lon = safe_float(point.group(1))
        lat = safe_float(point.group(2))
        return lat, lon
    pair = PAIR_RE.search(text)
    if pair:
        first = safe_float(pair.group(1))
        second = safe_float(pair.group(2))
        if first is None or second is None:
            return None, None
        if -90 <= first <= 90 and -180 <= second <= 180:
            return first, second
        if -90 <= second <= 90 and -180 <= first <= 180:
            return second, first
    return None, None


def extract_title_address(title) -> str | None:
    if pd.isna(title):
        return None
    match = TITLE_ADDRESS_RE.search(str(title))
    if not match:
        return None
    out = re.sub(r"\s+", " ", match.group(1)).strip(" -")
    return out or None


def has_house_number(text: str) -> bool:
    return bool(re.search(r"\b\d{1,6}\b", text or ""))


def split_address_variants(text: str | None) -> list[str]:
    if not text:
        return []
    base = re.sub(r"\s+", " ", str(text)).strip(" ,;")
    if not base:
        return []

    parts = [base]
    split_once: list[str] = []
    for part in parts:
        for item in MULTI_SPLIT_RE.split(part):
            cleaned = item.strip(" ,;")
            if cleaned:
                split_once.append(cleaned)

    final_parts: list[str] = []
    for part in split_once:
        slash_parts = [p.strip(" ,;") for p in re.split(r"\s*/\s*", part) if p.strip(" ,;")]
        if len(slash_parts) >= 2 and sum(1 for p in slash_parts if has_house_number(p)) >= 2:
            final_parts.extend(slash_parts)
            continue
        comma_parts = [p.strip(" ,;") for p in part.split(",") if p.strip(" ,;")]
        if len(comma_parts) >= 2 and sum(1 for p in comma_parts if has_house_number(p)) >= 2:
            final_parts.extend(comma_parts)
        else:
            final_parts.append(part)

    seen = set()
    deduped: list[str] = []
    for part in final_parts:
        key = part.upper()
        if key in seen:
            continue
        seen.add(key)
        deduped.append(part)
    return deduped


def standardize_address(raw: str | None) -> dict:
    if raw is None:
        return {
            "raw": None,
            "standardized": None,
            "house_number": None,
            "predir": None,
            "street_name": None,
            "street_type": None,
            "sufdir": None,
            "street_key_full": None,
            "street_key_name": None,
            "street_key_loose": None,
            "is_range": False,
            "range_start": None,
            "range_end": None,
        }

    text = str(raw).upper().replace("\u2013", "-").replace("\u2014", "-")
    text = apply_ocr_word_fixes(text)
    text = re.sub(r"^(\d{1,6})([NSEW])\b", r"\1 \2 ", text)
    text = re.sub(r"^(\d{1,6})(NORTH|SOUTH|EAST|WEST)\b", r"\1 \2 ", text)
    text = re.sub(r"\b(OF|FROM|TO)(NORTH|SOUTH|EAST|WEST)\b", r"\1 \2", text)
    text = UNIT_RE.sub("", text)
    text = text.replace(".", " ").replace(":", " ").replace(";", " ")
    text = re.sub(r"\s+", " ", text).strip()

    range_start = None
    range_end = None
    is_range = False
    range_match = LEADING_RANGE_RE.match(text)
    if range_match:
        range_start = int(range_match.group(1))
        range_end = int(range_match.group(2))
        is_range = True
        text = f"{range_match.group(1)} {text[range_match.end():].strip()}".strip()

    text = re.sub(r",?\s+CHICAGO\s*(,\s*)?IL(?:LINOIS)?\b", "", text)
    text = re.sub(r"\bIL(?:LINOIS)?\b", "", text)
    text = re.sub(r"\b\d{5}(?:-\d{4})?\b", "", text)
    text = re.sub(r"\s+", " ", text).strip(" ,")

    tokens = text.split()
    expanded: list[str] = []
    for token in tokens:
        expanded.append(DIRECTION_MAP.get(token, STREET_TYPE_MAP.get(token, token)))
    if len(expanded) >= 4 and expanded[-2] in FULL_STREET_TYPES and BARE_UNIT_RE.fullmatch(expanded[-1]):
        expanded = expanded[:-1]
    text = " ".join(expanded)
    text = re.sub(r"\s+", " ", text).strip()

    parsed = parse_address_components(text)
    return {
        "raw": raw,
        "standardized": text or None,
        "house_number": parsed.get("house_number"),
        "predir": parsed.get("predir"),
        "street_name": parsed.get("street_name"),
        "street_type": parsed.get("street_type"),
        "sufdir": parsed.get("sufdir"),
        "street_key_full": parsed.get("street_key_full"),
        "street_key_name": parsed.get("street_key_name"),
        "street_key_loose": parsed.get("street_key_loose"),
        "is_range": is_range,
        "range_start": range_start,
        "range_end": range_end,
    }


def parse_address_components(standardized: str | None) -> dict:
    if not standardized:
        return {
            "house_number": None,
            "predir": None,
            "street_name": None,
            "street_type": None,
            "sufdir": None,
            "street_key_full": None,
            "street_key_name": None,
            "street_key_loose": None,
        }

    tokens = standardized.split()
    if not tokens or not re.fullmatch(r"\d{1,6}", tokens[0]):
        return {
            "house_number": None,
            "predir": None,
            "street_name": None,
            "street_type": None,
            "sufdir": None,
            "street_key_full": None,
            "street_key_name": None,
            "street_key_loose": None,
        }

    house_number = int(tokens[0])
    idx = 1
    predir = None
    if idx < len(tokens) and tokens[idx] in FULL_DIRECTIONS:
        predir = tokens[idx]
        idx += 1

    end = len(tokens)
    sufdir = None
    if end - idx >= 2 and tokens[-1] in FULL_DIRECTIONS:
        sufdir = tokens[-1]
        end -= 1

    street_type = None
    if end - idx >= 2 and tokens[end - 1] in FULL_STREET_TYPES:
        street_type = tokens[end - 1]
        end -= 1

    street_name_tokens = tokens[idx:end]
    if not street_name_tokens:
        return {
            "house_number": house_number,
            "predir": predir,
            "street_name": None,
            "street_type": street_type,
            "sufdir": sufdir,
            "street_key_full": None,
            "street_key_name": None,
            "street_key_loose": None,
        }

    street_name = " ".join(street_name_tokens)
    street_key_full = "|".join([
        predir or "",
        street_name,
        street_type or "",
        sufdir or "",
    ])
    street_key_loose = "|".join([
        street_name,
        street_type or "",
        sufdir or "",
    ])
    street_key_name = "|".join([
        predir or "",
        street_name,
        sufdir or "",
    ])

    return {
        "house_number": house_number,
        "predir": predir,
        "street_name": street_name,
        "street_type": street_type,
        "sufdir": sufdir,
        "street_key_full": street_key_full,
        "street_key_name": street_key_name,
        "street_key_loose": street_key_loose,
    }


def build_candidate_addresses(row: pd.Series) -> list[dict]:
    candidates: list[dict] = []

    address_raw = row.get("address_raw")
    address_raw_loose = row.get("address_raw_loose")
    matter_title = row.get("matter_title")

    for source, candidate_text in [
        ("address_raw", address_raw),
        ("address_raw_loose", address_raw_loose),
        ("matter_title", extract_title_address(matter_title)),
    ]:
        for piece in split_address_variants(candidate_text):
            standardized = standardize_address(piece)
            std = standardized["standardized"]
            if not std:
                continue
            candidates.append(
                {
                    "candidate_source": source,
                    "candidate_raw": piece,
                    "candidate_standardized": std,
                    "house_number": standardized["house_number"],
                    "street_key_full": standardized["street_key_full"],
                    "street_key_name": standardized["street_key_name"],
                    "street_key_loose": standardized["street_key_loose"],
                    "is_range": standardized["is_range"],
                    "range_start": standardized["range_start"],
                    "range_end": standardized["range_end"],
                    "external_address": f"{piece}, Chicago, IL",
                }
            )

    seen = set()
    deduped: list[dict] = []
    for candidate in candidates:
        key = candidate["candidate_standardized"]
        if key in seen:
            continue
        seen.add(key)
        deduped.append(candidate)
    return deduped


def coerce_lat_lon(universe_df: pd.DataFrame) -> pd.DataFrame:
    columns = list(universe_df.columns)
    lat_col = choose_column(columns, ["latitude", "lat", "property_latitude"], contains=["lat"])
    lon_col = choose_column(columns, ["longitude", "lon", "lng", "long", "property_longitude"], contains=["lon"])
    if lon_col is None:
        lon_col = choose_column(columns, ["x", "x_coordinate"], contains=["long"])

    location_col = choose_column(columns, ["location", "geo_location", "point", "coordinates"])

    out = universe_df.copy()
    out["latitude"] = out[lat_col].map(safe_float) if lat_col else None
    out["longitude"] = out[lon_col].map(safe_float) if lon_col else None

    if location_col:
        parsed = out[location_col].map(parse_lat_lon_from_text)
        parsed_lat = parsed.map(lambda p: p[0])
        parsed_lon = parsed.map(lambda p: p[1])
        out["latitude"] = out["latitude"].where(out["latitude"].notna(), parsed_lat)
        out["longitude"] = out["longitude"].where(out["longitude"].notna(), parsed_lon)

    out = out[(out["latitude"].between(-90, 90)) & (out["longitude"].between(-180, 180))]
    return out


def read_parcel_tables(in_parcel_addresses_csv: str, in_parcel_universe_csv: str) -> tuple[pd.DataFrame, dict]:
    addresses_columns = pd.read_csv(in_parcel_addresses_csv, nrows=0).columns.tolist()
    universe_columns = pd.read_csv(in_parcel_universe_csv, nrows=0).columns.tolist()

    addr_pin_col = choose_column(addresses_columns, ["pin", "pin14", "parcel_pin", "parcelid"], contains=["pin"])
    addr_address_col = choose_column(
        addresses_columns,
        ["property_address", "prop_address", "site_address", "address"],
        contains=["address"],
    )
    addr_city_col = choose_column(addresses_columns, ["property_city", "city"], contains=["city"])
    addr_zip_col = choose_column(addresses_columns, ["property_zip", "zip", "zipcode"], contains=["zip"])

    if not addr_pin_col or not addr_address_col:
        raise ValueError("Could not identify PIN/address columns in parcel addresses dataset.")

    universe_pin_col = choose_column(universe_columns, ["pin", "pin14", "parcel_pin", "parcelid"], contains=["pin"])
    if not universe_pin_col:
        raise ValueError("Could not identify PIN column in parcel universe dataset.")

    universe_lat_col = choose_column(universe_columns, ["latitude", "lat", "property_latitude"], contains=["lat"])
    universe_lon_col = choose_column(universe_columns, ["longitude", "lon", "lng", "long", "property_longitude"], contains=["lon"])
    universe_location_col = choose_column(universe_columns, ["location", "geo_location", "point", "coordinates"])

    addr_usecols = [addr_pin_col, addr_address_col]
    if addr_city_col:
        addr_usecols.append(addr_city_col)
    if addr_zip_col:
        addr_usecols.append(addr_zip_col)

    universe_usecols = [universe_pin_col]
    for col in [universe_lat_col, universe_lon_col, universe_location_col]:
        if col and col not in universe_usecols:
            universe_usecols.append(col)

    addresses_df = pd.read_csv(in_parcel_addresses_csv, dtype=str, usecols=addr_usecols, low_memory=False)
    addresses_df = addresses_df.rename(columns={
        addr_pin_col: "pin",
        addr_address_col: "property_address",
    })
    if addr_city_col:
        addresses_df = addresses_df.rename(columns={addr_city_col: "property_city"})
    if addr_zip_col:
        addresses_df = addresses_df.rename(columns={addr_zip_col: "property_zip"})

    addresses_df["pin"] = addresses_df["pin"].map(normalize_pin)
    addresses_df = addresses_df[addresses_df["pin"].notna()]
    addresses_df["property_address"] = addresses_df["property_address"].fillna("").str.strip()
    addresses_df = addresses_df[addresses_df["property_address"] != ""]

    chicago_filter_applied = False
    if "property_city" in addresses_df.columns:
        city = addresses_df["property_city"].fillna("").str.upper().str.strip()
        chicago_filter_applied = (city == "CHICAGO").any()
        if chicago_filter_applied:
            addresses_df = addresses_df[city == "CHICAGO"]

    universe_df = pd.read_csv(in_parcel_universe_csv, dtype=str, usecols=universe_usecols, low_memory=False)
    universe_df = universe_df.rename(columns={universe_pin_col: "pin"})
    universe_df["pin"] = universe_df["pin"].map(normalize_pin)
    universe_df = universe_df[universe_df["pin"].notna()]
    universe_df = coerce_lat_lon(universe_df)

    if "latitude" not in universe_df.columns or "longitude" not in universe_df.columns:
        raise ValueError("Could not identify latitude/longitude columns in parcel universe dataset.")

    universe_df = universe_df[["pin", "latitude", "longitude"]].dropna(subset=["latitude", "longitude"])
    universe_df = universe_df.sort_values("pin").drop_duplicates(subset=["pin"], keep="first")

    merged = addresses_df.merge(universe_df, on="pin", how="inner")
    merged = merged.dropna(subset=["property_address", "latitude", "longitude"]).copy()

    unique_addresses = merged["property_address"].dropna().unique().tolist()
    standardized_map = {raw: standardize_address(raw) for raw in unique_addresses}

    merged["std_address"] = merged["property_address"].map(lambda v: standardized_map[v]["standardized"])
    merged["house_number"] = merged["property_address"].map(lambda v: standardized_map[v]["house_number"])
    merged["street_key_full"] = merged["property_address"].map(lambda v: standardized_map[v]["street_key_full"])
    merged["street_key_name"] = merged["property_address"].map(lambda v: standardized_map[v]["street_key_name"])
    merged["street_key_loose"] = merged["property_address"].map(lambda v: standardized_map[v]["street_key_loose"])

    merged = merged[merged["std_address"].notna()].copy()

    diagnostics = {
        "parcel_address_rows_input": int(len(addresses_df)),
        "parcel_universe_rows_with_coords": int(len(universe_df)),
        "parcel_rows_after_join": int(len(merged)),
        "parcel_unique_std_addresses": int(merged["std_address"].nunique()),
        "chicago_city_filter_applied": bool(chicago_filter_applied),
    }
    return merged, diagnostics


def build_exact_lookup(parcel_df: pd.DataFrame) -> dict:
    grouped = (
        parcel_df.groupby("std_address", as_index=False)
        .agg(
            matched_pin=("pin", "first"),
            latitude=("latitude", "median"),
            longitude=("longitude", "median"),
            parcel_count=("pin", "count"),
        )
    )
    out = {}
    for row in grouped.itertuples(index=False):
        out[row.std_address] = {
            "matched_pin": row.matched_pin,
            "latitude": float(row.latitude),
            "longitude": float(row.longitude),
            "parcel_count": int(row.parcel_count),
        }
    return out


def build_street_groups(parcel_df: pd.DataFrame, key_col: str) -> dict:
    if key_col not in parcel_df.columns:
        return {}
    usable = parcel_df[parcel_df[key_col].notna() & parcel_df["house_number"].notna()].copy()
    groups = {}
    for key, group in usable.groupby(key_col):
        columns = ["pin", "latitude", "longitude", "house_number", "std_address"]
        if "street_key_full" in group.columns:
            columns.append("street_key_full")
        groups[key] = group[columns].sort_values("house_number")
    return groups


def choose_row_for_house(group: pd.DataFrame, house_number: int) -> dict | None:
    candidates = group[group["house_number"] == house_number]
    if candidates.empty:
        return None
    chosen = candidates.sort_values(["pin"]).iloc[0]
    return {
        "matched_pin": chosen["pin"],
        "latitude": float(chosen["latitude"]),
        "longitude": float(chosen["longitude"]),
        "matched_std_address": chosen["std_address"],
    }


def choose_row_nearest_house(group: pd.DataFrame, target: int, max_diff: int) -> dict | None:
    if group.empty:
        return None
    g = group.copy()
    g["diff"] = (g["house_number"] - target).abs()
    min_diff = int(g["diff"].min())
    if min_diff > max_diff:
        return None
    best = g[g["diff"] == min_diff]
    if len(best) != 1:
        if best["std_address"].dropna().nunique() != 1:
            return None
        best = best.sort_values(["pin"]).head(1)
    chosen = best.iloc[0]
    return {
        "matched_pin": chosen["pin"],
        "latitude": float(chosen["latitude"]),
        "longitude": float(chosen["longitude"]),
        "matched_std_address": chosen["std_address"],
        "house_diff": min_diff,
    }


def choose_row_for_house_name_fallback(group: pd.DataFrame, house_number: int) -> dict | None:
    if "street_key_full" not in group.columns:
        return None
    candidates = group[group["house_number"] == house_number]
    if candidates.empty:
        return None
    if candidates["street_key_full"].dropna().nunique() != 1:
        return None
    return choose_row_for_house(candidates, house_number)


def choose_row_for_range(group: pd.DataFrame, start: int, end: int) -> dict | None:
    if group.empty:
        return None
    low = min(start, end)
    high = max(start, end)
    midpoint = (low + high) / 2.0

    in_range = group[(group["house_number"] >= low) & (group["house_number"] <= high)]
    if in_range.empty:
        block = low // 100
        in_range = group[(group["house_number"] // 100) == block]
    if in_range.empty:
        return None

    g = in_range.copy()
    g["mid_diff"] = (g["house_number"] - midpoint).abs()
    chosen = g.sort_values(["mid_diff", "pin"]).iloc[0]
    return {
        "matched_pin": chosen["pin"],
        "latitude": float(chosen["latitude"]),
        "longitude": float(chosen["longitude"]),
        "matched_std_address": chosen["std_address"],
    }


def match_candidate(
    candidate: dict,
    exact_lookup: dict,
    street_groups_full: dict,
    street_groups_name: dict,
    street_groups_loose: dict,
    nearest_max_diff: int,
) -> dict | None:
    std_address = candidate["candidate_standardized"]
    if std_address in exact_lookup:
        hit = exact_lookup[std_address]
        return {
            "matched_pin": hit["matched_pin"],
            "latitude": hit["latitude"],
            "longitude": hit["longitude"],
            "geocode_confidence": "exact",
            "match_method": "exact_std_address",
            "matched_std_address": std_address,
        }

    house_number = candidate.get("house_number")
    full_key = candidate.get("street_key_full")
    name_key = candidate.get("street_key_name")
    loose_key = candidate.get("street_key_loose")

    group = street_groups_full.get(full_key) if full_key else None
    if group is None or group.empty:
        group = street_groups_loose.get(loose_key) if loose_key else None

    if group is not None and house_number is not None:
        exact_house = choose_row_for_house(group, house_number)
        if exact_house:
            return {
                **exact_house,
                "geocode_confidence": "fuzzy_address",
                "match_method": "house_and_street_exact",
            }

        nearest = choose_row_nearest_house(group, house_number, max_diff=nearest_max_diff)
        if nearest:
            return {
                **nearest,
                "geocode_confidence": "fuzzy_address",
                "match_method": "house_and_street_nearest",
            }

    name_group = street_groups_name.get(name_key) if name_key else None
    if name_group is not None and house_number is not None:
        name_fallback = choose_row_for_house_name_fallback(name_group, house_number)
        if name_fallback:
            return {
                **name_fallback,
                "geocode_confidence": "fuzzy_address",
                "match_method": "house_and_street_name",
            }

    if candidate.get("is_range") and group is not None:
        start = candidate.get("range_start")
        end = candidate.get("range_end")
        if start is not None and end is not None:
            range_hit = choose_row_for_range(group, start=start, end=end)
            if range_hit:
                return {
                    **range_hit,
                    "geocode_confidence": "fuzzy_block",
                    "match_method": "range_block",
                }

    return None


def run_stage1(
    rezoning_df: pd.DataFrame,
    parcel_df: pd.DataFrame,
    nearest_max_diff: int,
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, dict]:
    exact_lookup = build_exact_lookup(parcel_df)
    street_groups_full = build_street_groups(parcel_df, key_col="street_key_full")
    street_groups_name = build_street_groups(parcel_df, key_col="street_key_name")
    street_groups_loose = build_street_groups(parcel_df, key_col="street_key_loose")

    outputs = rezoning_df.copy()
    outputs["external_row_id"] = outputs["matter_id"].astype(str)

    all_candidate_rows: list[dict] = []
    match_records: list[dict] = []
    unmatched_records: list[dict] = []

    for idx, row in outputs.iterrows():
        matter_id = row.get("matter_id")
        candidates = build_candidate_addresses(row)
        best_match = None
        best_candidate = None

        for rank, candidate in enumerate(candidates, start=1):
            candidate["matter_id"] = matter_id
            candidate["candidate_rank"] = rank
            all_candidate_rows.append(candidate)

            matched = match_candidate(
                candidate=candidate,
                exact_lookup=exact_lookup,
                street_groups_full=street_groups_full,
                street_groups_name=street_groups_name,
                street_groups_loose=street_groups_loose,
                nearest_max_diff=nearest_max_diff,
            )
            if matched:
                best_match = matched
                best_candidate = candidate
                break

        if best_match:
            outputs.at[idx, "latitude"] = best_match["latitude"]
            outputs.at[idx, "longitude"] = best_match["longitude"]
            outputs.at[idx, "matched_pin"] = best_match["matched_pin"]
            outputs.at[idx, "geocode_source"] = "parcel_match"
            outputs.at[idx, "geocode_confidence"] = best_match["geocode_confidence"]
            outputs.at[idx, "match_method"] = best_match["match_method"]
            outputs.at[idx, "matched_address_std"] = best_match.get("matched_std_address")
            outputs.at[idx, "address_for_geocoding"] = best_candidate["external_address"] if best_candidate else None
            outputs.at[idx, "matched_address_candidate_std"] = (
                best_candidate["candidate_standardized"] if best_candidate else None
            )
            outputs.at[idx, "address_source_used"] = best_candidate["candidate_source"] if best_candidate else None
            match_records.append(
                {
                    "matter_id": matter_id,
                    "external_row_id": str(matter_id),
                    "matched_pin": best_match["matched_pin"],
                    "latitude": best_match["latitude"],
                    "longitude": best_match["longitude"],
                    "geocode_confidence": best_match["geocode_confidence"],
                    "match_method": best_match["match_method"],
                    "address_source_used": best_candidate["candidate_source"] if best_candidate else None,
                    "candidate_standardized": best_candidate["candidate_standardized"] if best_candidate else None,
                    "address_for_geocoding": best_candidate["external_address"] if best_candidate else None,
                }
            )
        else:
            outputs.at[idx, "latitude"] = None
            outputs.at[idx, "longitude"] = None
            outputs.at[idx, "matched_pin"] = None
            outputs.at[idx, "geocode_source"] = "unmatched"
            outputs.at[idx, "geocode_confidence"] = None
            outputs.at[idx, "match_method"] = None
            outputs.at[idx, "matched_address_std"] = None
            outputs.at[idx, "matched_address_candidate_std"] = None
            outputs.at[idx, "address_source_used"] = None

            fallback_external = candidates[0]["external_address"] if candidates else None
            outputs.at[idx, "address_for_geocoding"] = fallback_external
            unmatched_records.append(
                {
                    "matter_id": matter_id,
                    "external_row_id": str(matter_id),
                    "address_for_external": fallback_external,
                    "street_address": candidates[0]["candidate_raw"] if candidates else None,
                    "city": "Chicago",
                    "state": "IL",
                    "zip": None,
                    "address_candidate_count": len(candidates),
                }
            )

    all_candidates_df = pd.DataFrame(all_candidate_rows)
    matched_df = pd.DataFrame(match_records)
    unmatched_df = pd.DataFrame(unmatched_records)

    matched_count = int((outputs["geocode_source"] == "parcel_match").sum())
    total_count = int(len(outputs))
    addressable_matters = int(all_candidates_df["matter_id"].nunique()) if not all_candidates_df.empty else 0

    summary = {
        "timestamp_utc": datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z"),
        "total_rezoning_rows": total_count,
        "rows_with_any_address_candidate": addressable_matters,
        "stage1_matched_rows": matched_count,
        "stage1_unmatched_rows": int(total_count - matched_count),
        "stage1_match_rate_over_all": (matched_count / total_count) if total_count else None,
        "stage1_match_rate_over_addressable": (matched_count / addressable_matters) if addressable_matters else None,
        "match_confidence_counts": (
            outputs["geocode_confidence"].fillna("unmatched").value_counts().to_dict()
        ),
        "match_method_counts": outputs["match_method"].fillna("unmatched").value_counts().to_dict(),
        "address_source_counts": outputs["address_source_used"].fillna("none").value_counts().to_dict(),
    }

    return outputs, unmatched_df, all_candidates_df, summary


def main() -> int:
    args = parse_args()

    rezoning_df = pd.read_csv(args.in_rezoning_csv, dtype=str, low_memory=False)
    parcel_df, parcel_diag = read_parcel_tables(
        in_parcel_addresses_csv=args.in_parcel_addresses_csv,
        in_parcel_universe_csv=args.in_parcel_universe_csv,
    )

    stage1_df, unmatched_df, candidates_df, summary = run_stage1(
        rezoning_df=rezoning_df,
        parcel_df=parcel_df,
        nearest_max_diff=args.nearest_max_diff,
    )

    ensure_parent(args.out_stage1_csv)
    ensure_parent(args.out_unmatched_csv)

    stage1_df.to_csv(args.out_stage1_csv, index=False)
    unmatched_df.to_csv(args.out_unmatched_csv, index=False)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
