import argparse
import json
import math
import os
import re
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

try:
    import geopandas as gpd
except Exception:  # noqa: BLE001
    gpd = None

try:
    import fiona
except Exception:  # noqa: BLE001
    fiona = None

try:
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
except Exception:  # noqa: BLE001
    plt = None

DATE_PRE_2015_END = pd.Timestamp("2015-05-18")
DATE_POST_2023_START = pd.Timestamp("2023-05-16")

THREE_PART_ZONE_RE = re.compile(r"\b([A-Z]{1,3})\s*-\s*(\d)\s*-\s*(\d+(?:\.\d+)?[A-Z]?)\b")
ZONING_CODE_RE = re.compile(r"\b([A-Z]{1,4}\d?)\s*[- ]?\s*(\d+(?:\.\d+)?)([A-Z]?)\b")

PD_FAR_REVIEW_REQUIRED_COLUMNS = [
    "matter_id",
    "pd_far_update_source",
    "pd_transition_type",
    "from_far_update",
    "to_far_update",
    "far_change_update",
    "far_direction_update",
    "pd_far_value_update",
    "pd_far_role_update",
    "confidence",
    "evidence",
    "notes",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-geocoded-csv", required=True)
    parser.add_argument("--in-zoning-lookup-csv", required=True)
    parser.add_argument("--in-ward-gpkg", required=True)
    parser.add_argument("--ward-layer-pre2015")
    parser.add_argument("--ward-layer-2015")
    parser.add_argument("--ward-layer-post2023")
    parser.add_argument("--out-final-csv", required=True)
    parser.add_argument("--out-ward-counts-csv", required=True)
    parser.add_argument("--out-sponsor-validation-csv", required=True)
    parser.add_argument("--out-map-source-pdf", required=True)
    parser.add_argument("--out-map-upzone-pdf", required=True)
    parser.add_argument("--out-summary-json")
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


def ward_era_for_date(value) -> str | None:
    ts = pd.to_datetime(value, errors="coerce")
    if pd.isna(ts):
        return None
    if ts < DATE_PRE_2015_END:
        return "pre_2015"
    if ts < DATE_POST_2023_START:
        return "2015_to_2023"
    return "post_2023"


def clean_zone_text(value) -> str | None:
    if pd.isna(value):
        return None
    text = str(value).upper().strip()
    if not text:
        return None
    text = text.replace("\u2013", "-").replace("\u2014", "-")
    text = re.sub(r"^\s*[NA]\s+(?=[A-Z0-9])", "", text)
    text = re.sub(r"\b([A-Z]{1,4}\d?)\s+(\d+(?:\.\d+)?)\b", r"\1-\2", text)
    text = re.sub(r"-L\b", "-1", text)
    text = re.sub(r"\s+", " ", text)
    return text.strip(" ,.;") or None


def parse_zoning_code(value) -> str | None:
    text = clean_zone_text(value)
    if not text:
        return None

    if "PLANNED DEVELOPMENT" in text:
        return "PD"
    if re.search(r"\bPD\b", text):
        return "PD"
    if (
        re.search(r"\bPOS\b", text)
        or re.search(r"\bPARKS?\s+AND\s+OPEN\s+SPACE\b", text)
        or re.search(r"\bOPEN\s+SPACE\s+DISTRICT\b", text)
    ):
        return "POS"
    if text == "T" or text.startswith("T TRANSPORTATION"):
        return "T"

    old_r_match = re.search(r"\bR([1-8])\b", text)
    if old_r_match:
        return f"R{old_r_match.group(1)}"

    c4_match = re.search(r"\bC\s*[- ]?4\b", text)
    if c4_match:
        return "C4"

    code = None
    three_part = THREE_PART_ZONE_RE.search(text)
    if three_part:
        code = f"{three_part.group(1)}{three_part.group(2)}-{three_part.group(3)}"
    else:
        match = ZONING_CODE_RE.search(text)
        if not match:
            return None

        prefix = match.group(1)
        number = match.group(2)
        suffix = match.group(3)
        code = f"{prefix}-{number}{suffix}"

    code = code.replace("--", "-")
    code = re.sub(r"\s+", "", code)
    code = code.replace("-L", "-1")
    code = code.replace("B1.4", "B1-4")
    code = code.replace("M1.-4", "M1-4")
    code = code.replace("B24", "B2-4")
    code = code.replace("C24", "C2-4")
    code = code.replace("MI-", "M1-")
    code = code.replace("RT-4-5", "RT-4.5")
    code = re.sub(r"^((?:B[1-7]|C[1-5]|M[1-3]|BL|CL|CI|ML)-\d+(?:\.\d+)?)[A-Z]$", r"\1", code)
    code = re.sub(r"^(RM|RT|RS)(\d)-5$", r"\1-\2.5", code)
    code = re.sub(r"^R-(\d)$", r"R\1", code)
    code = re.sub(r"^C-4$", "C4", code)

    if code == "RS3":
        code = "RS-3"
    if code == "RM4.5":
        code = "RM-4.5"
    if code == "RM-4":
        code = "RM-4.5"

    return code


def parse_lookup_date(value) -> pd.Timestamp | None:
    ts = pd.to_datetime(value, errors="coerce")
    if pd.isna(ts):
        return None
    return ts


def format_lookup_date(value: pd.Timestamp | None) -> str | None:
    if value is None or pd.isna(value):
        return None
    return value.strftime("%Y-%m-%d")


def load_far_lookup(path: str) -> tuple[dict[str, list[dict]], dict]:
    df = pd.read_csv(path, dtype=str, low_memory=False)
    cols = df.columns.tolist()

    code_col = choose_column(
        cols,
        ["zone_code", "district", "district_type", "code", "zoning_code"],
        contains=["district"],
    )
    if code_col is None:
        code_col = cols[0]

    far_col = choose_column(cols, ["max_far", "far", "maximum_far", "floor_area_ratio"], contains=["far"])
    if far_col is None:
        raise ValueError("Could not identify FAR column in zoning lookup table.")

    start_col = choose_column(cols, ["effective_start_date", "start_date", "valid_from"], contains=["start", "date"])
    end_col = choose_column(cols, ["effective_end_date", "end_date", "valid_to"], contains=["end", "date"])
    version_col = choose_column(cols, ["zoning_code_version", "lookup_version", "version"], contains=["version"])

    lookup: dict[str, list[dict]] = {}
    for _, row in df.iterrows():
        zone_raw = row.get(code_col)
        far_raw = row.get(far_col)
        code = parse_zoning_code(zone_raw)
        far = safe_float(far_raw)
        if code and far is not None:
            record = {
                "far": far,
                "effective_start": parse_lookup_date(row.get(start_col)) if start_col else None,
                "effective_end": parse_lookup_date(row.get(end_col)) if end_col else None,
                "zoning_code_version": str(row.get(version_col)).strip() if version_col and pd.notna(row.get(version_col)) else None,
            }
            lookup.setdefault(code, []).append(record)

    has_effective_dates = bool(start_col or end_col)

    def append_manual(code: str, far: float) -> None:
        if has_effective_dates:
            record = {
                "far": far,
                "effective_start": pd.Timestamp("2004-11-01"),
                "effective_end": None,
                "zoning_code_version": "post_2004_manual",
            }
        else:
            record = {
                "far": far,
                "effective_start": None,
                "effective_end": None,
                "zoning_code_version": "manual",
            }
        lookup.setdefault(code, []).append(record)

    # Manual extensions from project notes.
    for prefix, values in {
        "BL": {"1": 1.2, "1.5": 1.5, "2": 2.2, "3": 3.0, "5": 5.0},
        "CI": {"1": 1.2, "1.5": 1.5, "2": 2.2, "3": 3.0, "5": 5.0},
        "CL": {"1": 1.2, "1.5": 1.5, "2": 2.2, "3": 3.0, "5": 5.0},
        "ML": {"1": 1.2, "1.5": 1.5, "2": 2.2, "3": 3.0, "5": 5.0},
    }.items():
        for suffix, far in values.items():
            append_manual(f"{prefix}-{suffix}", far)

    append_manual("RT-4A", 1.2)
    append_manual("RM-4.5", 1.7)
    append_manual("T", 1.5)

    for code, records in lookup.items():
        records.sort(
            key=lambda rec: (
                rec["effective_start"] is None,
                rec["effective_start"] if rec["effective_start"] is not None else pd.Timestamp.max,
            )
        )
        lookup[code] = records

    diag = {
        "lookup_rows": int(len(df)),
        "lookup_codes_loaded": int(len(lookup)),
        "code_col": code_col,
        "far_col": far_col,
        "effective_start_col": start_col,
        "effective_end_col": end_col,
        "zoning_code_version_col": version_col,
        "has_effective_dates": has_effective_dates,
    }
    return lookup, diag


def far_for_code(code: str | None, lookup: dict[str, list[dict]], matter_intro_date=None) -> tuple[float | None, str | None, str | None, str | None]:
    if not code:
        return None, None, None, None
    if code in {"PD", "POS"}:
        return None, None, None, None

    records = lookup.get(code)
    if not records:
        return None, None, None, None

    intro_ts = pd.to_datetime(matter_intro_date, errors="coerce")
    chosen = None

    if not pd.isna(intro_ts):
        applicable: list[dict] = []
        for rec in records:
            start = rec.get("effective_start")
            end = rec.get("effective_end")
            if start is not None and intro_ts < start:
                continue
            if end is not None and intro_ts > end:
                continue
            applicable.append(rec)

        if applicable:
            applicable.sort(
                key=lambda rec: rec.get("effective_start") if rec.get("effective_start") is not None else pd.Timestamp.min,
                reverse=True,
            )
            chosen = applicable[0]

    if chosen is None:
        chosen = records[0]

    return (
        chosen.get("far"),
        chosen.get("zoning_code_version"),
        format_lookup_date(chosen.get("effective_start")),
        format_lookup_date(chosen.get("effective_end")),
    )


def assign_far_columns(df: pd.DataFrame, lookup: dict[str, list[dict]]) -> pd.DataFrame:
    out = df.copy()
    if {"from_code", "to_code", "from_far", "to_far"}.issubset(out.columns):
        out["from_lookup_version"] = out["from_far_version"] if "from_far_version" in out.columns else None
        out["to_lookup_version"] = out["to_far_version"] if "to_far_version" in out.columns else None
        out["from_far_effective_start_date"] = out["from_far_effective_start"] if "from_far_effective_start" in out.columns else None
        out["from_far_effective_end_date"] = out["from_far_effective_end"] if "from_far_effective_end" in out.columns else None
        out["to_far_effective_start_date"] = out["to_far_effective_start"] if "to_far_effective_start" in out.columns else None
        out["to_far_effective_end_date"] = out["to_far_effective_end"] if "to_far_effective_end" in out.columns else None
    else:
        out["from_code"] = out["from_zoning"].map(parse_zoning_code)
        out["to_code"] = out["to_zoning"].map(parse_zoning_code)

        from_far_payload = out.apply(
            lambda row: far_for_code(
                code=row.get("from_code"),
                lookup=lookup,
                matter_intro_date=row.get("matter_intro_date"),
            ),
            axis=1,
        )
        to_far_payload = out.apply(
            lambda row: far_for_code(
                code=row.get("to_code"),
                lookup=lookup,
                matter_intro_date=row.get("matter_intro_date"),
            ),
            axis=1,
        )

        out["from_far"] = from_far_payload.map(lambda x: x[0])
        out["from_lookup_version"] = from_far_payload.map(lambda x: x[1])
        out["from_far_effective_start_date"] = from_far_payload.map(lambda x: x[2])
        out["from_far_effective_end_date"] = from_far_payload.map(lambda x: x[3])

        out["to_far"] = to_far_payload.map(lambda x: x[0])
        out["to_lookup_version"] = to_far_payload.map(lambda x: x[1])
        out["to_far_effective_start_date"] = to_far_payload.map(lambda x: x[2])
        out["to_far_effective_end_date"] = to_far_payload.map(lambda x: x[3])

    from_far_num = pd.to_numeric(out["from_far"], errors="coerce")
    to_far_num = pd.to_numeric(out["to_far"], errors="coerce")
    out["from_far"] = from_far_num
    out["to_far"] = to_far_num
    out["far_change"] = to_far_num - from_far_num
    out["is_upzone"] = out["far_change"].map(lambda x: None if pd.isna(x) else bool(x > 0))

    out["far_pair_status"] = "missing_one_side"
    out.loc[from_far_num.notna() & to_far_num.notna(), "far_pair_status"] = "resolved_both"
    out.loc[from_far_num.isna() & to_far_num.isna(), "far_pair_status"] = "missing_both"
    return out


def far_direction_from_change(value) -> str:
    change = safe_float(value)
    if change is None:
        return "unknown"
    if change > 0:
        return "upzone"
    if change < 0:
        return "downzone"
    return "same"


def add_far_direction(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out["far_direction"] = [far_direction_from_change(value) for value in out["far_change"]]
    return out


def apply_pd_far_review_updates(df: pd.DataFrame, in_pd_far_updates_csv: str) -> pd.DataFrame:
    out = df.copy()
    updates = pd.read_csv(in_pd_far_updates_csv, dtype=str, low_memory=False)

    missing_cols = [col for col in PD_FAR_REVIEW_REQUIRED_COLUMNS if col not in updates.columns]
    if missing_cols:
        raise ValueError(f"PD FAR review update file is missing columns: {', '.join(missing_cols)}")
    if "matter_id" not in out.columns:
        raise ValueError("Geocoded rezoning file is missing matter_id")
    if out["matter_id"].duplicated().any():
        duplicated = sorted(out.loc[out["matter_id"].duplicated(), "matter_id"].astype(str).unique())
        raise ValueError(f"Duplicate geocoded matter_id rows block PD FAR review updates: {duplicated}")
    if updates["matter_id"].duplicated().any():
        duplicated = sorted(updates.loc[updates["matter_id"].duplicated(), "matter_id"].astype(str).unique())
        raise ValueError(f"Duplicate PD FAR review update matter_id rows: {duplicated}")

    missing_ids = sorted(set(updates["matter_id"].astype(str)) - set(out["matter_id"].astype(str)))
    if missing_ids:
        raise ValueError(f"PD FAR review updates missing from geocoded data: {missing_ids}")

    updates = updates.set_index("matter_id")
    from_update = pd.to_numeric(updates["from_far_update"], errors="coerce")
    to_update = pd.to_numeric(updates["to_far_update"], errors="coerce")
    change_update = pd.to_numeric(updates["far_change_update"], errors="coerce")
    if from_update.isna().any() or to_update.isna().any() or change_update.isna().any():
        bad = sorted(updates.loc[from_update.isna() | to_update.isna() | change_update.isna()].index.astype(str).tolist())
        raise ValueError(f"PD FAR review updates have missing numeric FAR fields: {bad}")

    expected_change = to_update - from_update
    bad_change = (expected_change - change_update).abs() > 0.01
    if bad_change.any():
        bad = sorted(updates.loc[bad_change].index.astype(str).tolist())
        raise ValueError(f"PD FAR review updates have inconsistent FAR changes: {bad}")

    expected_direction = change_update.map(far_direction_from_change)
    bad_direction = updates["far_direction_update"].astype(str).str.strip().ne(expected_direction)
    if bad_direction.any():
        bad = sorted(updates.loc[bad_direction].index.astype(str).tolist())
        raise ValueError(f"PD FAR review updates have inconsistent FAR directions: {bad}")

    out["from_far"] = pd.to_numeric(out["from_far"], errors="coerce")
    out["to_far"] = pd.to_numeric(out["to_far"], errors="coerce")
    out["far_change"] = pd.to_numeric(out["far_change"], errors="coerce")

    out["pd_far_review_applied"] = False
    out["pd_far_update_source"] = ""
    out["pd_far_update_confidence"] = ""
    out["pd_far_transition_type"] = ""
    out["pd_far_value_update"] = pd.NA
    out["pd_far_role_update"] = ""
    out["pd_far_review_evidence"] = ""
    out["pd_far_review_notes"] = ""
    out["from_far_pre_pd_review"] = pd.to_numeric(out["from_far"], errors="coerce")
    out["to_far_pre_pd_review"] = pd.to_numeric(out["to_far"], errors="coerce")
    out["far_change_pre_pd_review"] = pd.to_numeric(out["far_change"], errors="coerce")
    out["far_pair_status_pre_pd_review"] = out["far_pair_status"]
    out["far_direction_pre_pd_review"] = [
        far_direction_from_change(value) for value in out["far_change_pre_pd_review"]
    ]

    update_mask = out["matter_id"].isin(updates.index)
    update_ids = out.loc[update_mask, "matter_id"]
    out.loc[update_mask, "pd_far_review_applied"] = True
    out.loc[update_mask, "pd_far_update_source"] = update_ids.map(updates["pd_far_update_source"])
    out.loc[update_mask, "pd_far_update_confidence"] = update_ids.map(updates["confidence"])
    out.loc[update_mask, "pd_far_transition_type"] = update_ids.map(updates["pd_transition_type"])
    out.loc[update_mask, "pd_far_value_update"] = update_ids.map(pd.to_numeric(updates["pd_far_value_update"], errors="coerce"))
    out.loc[update_mask, "pd_far_role_update"] = update_ids.map(updates["pd_far_role_update"])
    out.loc[update_mask, "pd_far_review_evidence"] = update_ids.map(updates["evidence"])
    out.loc[update_mask, "pd_far_review_notes"] = update_ids.map(updates["notes"])
    out.loc[update_mask, "from_far"] = update_ids.map(from_update)
    out.loc[update_mask, "to_far"] = update_ids.map(to_update)
    out.loc[update_mask, "far_change"] = update_ids.map(change_update)
    out.loc[update_mask, "far_pair_status"] = "resolved_both"
    out["is_upzone"] = [
        None if pd.isna(value) else bool(value > 0)
        for value in pd.to_numeric(out["far_change"], errors="coerce")
    ]
    return out


def list_layers(gpkg_path: str) -> list[str]:
    if fiona is not None:
        try:
            return list(fiona.listlayers(gpkg_path))
        except Exception:  # noqa: BLE001
            pass

    if gpd is not None:
        list_layers_fn = getattr(gpd, "list_layers", None)
        if callable(list_layers_fn):
            try:
                layer_df = list_layers_fn(gpkg_path)
                if "name" in layer_df:
                    layer_names = layer_df["name"].astype(str).tolist()
                    if layer_names:
                        return layer_names
            except Exception:  # noqa: BLE001
                pass

    # Final fallback for engines that can read default layers but cannot enumerate them.
    return ["__default__"]


def pick_layer(layers: list[str], explicit: str | None, keyword_candidates: list[str]) -> str | None:
    if explicit:
        return explicit
    lowered = [(layer, layer.lower()) for layer in layers]
    for keyword in keyword_candidates:
        for original, lower in lowered:
            if keyword in lower:
                return original
    return layers[0] if len(layers) == 1 else None


def choose_ward_column(columns: list[str]) -> str | None:
    return choose_column(columns, ["ward", "ward_num", "ward_number", "aldermanic_ward"], contains=["ward"])


def choose_year_column(columns: list[str]) -> str | None:
    return choose_column(columns, ["year", "map_year", "boundary_year"], contains=["year"])


def choose_panel_year(years: list[int], preferred: int, low: int | None = None, high: int | None = None) -> int | None:
    candidates = years
    if low is not None:
        candidates = [year for year in candidates if year >= low]
    if high is not None:
        candidates = [year for year in candidates if year <= high]
    if not candidates:
        return None
    if preferred in candidates:
        return preferred
    return min(candidates, key=lambda year: abs(year - preferred))


def normalize_ward_geoms(wards: "gpd.GeoDataFrame", source_label: str, warnings: list[str]) -> "gpd.GeoDataFrame | None":
    if wards is None or wards.empty:
        warnings.append(f"Ward boundaries are empty for {source_label}.")
        return None

    ward_col = choose_ward_column(list(wards.columns))
    if ward_col is None:
        warnings.append(f"Could not identify ward column in {source_label}.")
        return None

    if wards.crs is None:
        wards = wards.set_crs("EPSG:4326")

    out = wards[[ward_col, "geometry"]].copy()
    out = out.rename(columns={ward_col: "ward"})
    out["ward"] = out["ward"].astype(str).str.extract(r"(\d+)", expand=False)
    out = out[out["ward"].notna()].copy()
    if out.empty:
        warnings.append(f"No numeric ward values found in {source_label}.")
        return None
    return out


def build_era_ward_map_from_annual_panel(
    ward_panel: "gpd.GeoDataFrame",
    layer_name: str,
    warnings: list[str],
) -> tuple[dict[str, "gpd.GeoDataFrame | None"], dict]:
    year_col = choose_year_column(list(ward_panel.columns))
    if year_col is None:
        warnings.append(
            "Ward GPKG has a single layer but no year column; "
            "falling back to one shared geometry for all eras."
        )
        normalized = normalize_ward_geoms(ward_panel, source_label=layer_name, warnings=warnings)
        return (
            {
                "pre_2015": normalized,
                "2015_to_2023": normalized,
                "post_2023": normalized,
            },
            {"mode": "single_layer_no_year", "layer": layer_name},
        )

    year_values = pd.to_numeric(ward_panel[year_col], errors="coerce").dropna().astype(int).unique().tolist()
    year_values = sorted(year_values)
    if not year_values:
        warnings.append("Ward year column exists but has no valid years.")
        return (
            {
                "pre_2015": None,
                "2015_to_2023": None,
                "post_2023": None,
            },
            {"mode": "single_layer_year_panel", "layer": layer_name, "selected_years": {}},
        )

    selected_years = {
        "pre_2015": choose_panel_year(year_values, preferred=2014, high=2014),
        "2015_to_2023": choose_panel_year(year_values, preferred=2015, low=2015, high=2023),
        "post_2023": choose_panel_year(year_values, preferred=2024, low=2024),
    }

    era_map: dict[str, "gpd.GeoDataFrame | None"] = {}
    for era, selected_year in selected_years.items():
        if selected_year is None:
            warnings.append(f"No ward panel year available for era {era}.")
            era_map[era] = None
            continue

        subset = ward_panel[pd.to_numeric(ward_panel[year_col], errors="coerce") == selected_year].copy()
        era_map[era] = normalize_ward_geoms(
            subset,
            source_label=f"{layer_name} (year={selected_year})",
            warnings=warnings,
        )

    diag = {
        "mode": "single_layer_year_panel",
        "layer": layer_name,
        "year_col": year_col,
        "available_years": year_values,
        "selected_years": selected_years,
    }
    return era_map, diag


def build_era_ward_map_from_layers(
    gpkg_path: str,
    layers: list[str],
    layer_pre2015: str | None,
    layer_2015: str | None,
    layer_post2023: str | None,
    warnings: list[str],
) -> tuple[dict[str, "gpd.GeoDataFrame | None"], dict]:
    layer_map = {
        "pre_2015": pick_layer(layers, layer_pre2015, ["pre", "2014", "2011", "2010", "2003"]),
        "2015_to_2023": pick_layer(layers, layer_2015, ["2015", "ward_2015"]),
        "post_2023": pick_layer(layers, layer_post2023, ["2024", "2023", "new"]),
    }

    era_map: dict[str, "gpd.GeoDataFrame | None"] = {}
    for era, layer in layer_map.items():
        if layer is None:
            warnings.append(f"No ward layer resolved for era {era}.")
            era_map[era] = None
            continue
        wards = gpd.read_file(gpkg_path, layer=layer)
        era_map[era] = normalize_ward_geoms(wards, source_label=f"layer={layer}", warnings=warnings)

    diag = {"mode": "multi_layer", "layer_map": layer_map}
    return era_map, diag


def assign_wards_and_distance(
    df: pd.DataFrame,
    ward_gpkg_path: str,
    layer_pre2015: str | None,
    layer_2015: str | None,
    layer_post2023: str | None,
) -> tuple[pd.DataFrame, dict, list[str]]:
    warnings: list[str] = []

    out = df.copy()
    out["ward_era"] = out["matter_intro_date"].map(ward_era_for_date)
    out["ward"] = None
    out["dist_to_boundary_ft"] = None

    if gpd is None:
        warnings.append("geopandas is not installed; ward assignment and boundary distance were skipped.")
        return out, {"ward_assignment_enabled": False}, warnings

    if not os.path.exists(ward_gpkg_path):
        warnings.append("Ward boundary GPKG file not found; ward assignment and distance were skipped.")
        return out, {"ward_assignment_enabled": False}, warnings

    layers = list_layers(ward_gpkg_path)
    if not layers:
        warnings.append("No layers found in ward GPKG.")
        return out, {"ward_assignment_enabled": False}, warnings

    first_layer = layers[0]
    if first_layer == "__default__":
        first_layer_gdf = gpd.read_file(ward_gpkg_path)
        first_layer_label = "default_layer"
    else:
        first_layer_gdf = gpd.read_file(ward_gpkg_path, layer=first_layer)
        first_layer_label = first_layer
    first_year_col = choose_year_column(list(first_layer_gdf.columns))

    if len(layers) == 1 or first_year_col is not None:
        era_ward_map, ward_source_diag = build_era_ward_map_from_annual_panel(
            ward_panel=first_layer_gdf,
            layer_name=first_layer_label,
            warnings=warnings,
        )
    else:
        era_ward_map, ward_source_diag = build_era_ward_map_from_layers(
            gpkg_path=ward_gpkg_path,
            layers=layers,
            layer_pre2015=layer_pre2015,
            layer_2015=layer_2015,
            layer_post2023=layer_post2023,
            warnings=warnings,
        )

    lat = pd.to_numeric(out.get("latitude"), errors="coerce")
    lon = pd.to_numeric(out.get("longitude"), errors="coerce")
    valid_points = lat.notna() & lon.notna() & lat.between(-90, 90) & lon.between(-180, 180)

    if not valid_points.any():
        warnings.append("No valid latitude/longitude rows available for ward assignment.")
        return out, {"ward_assignment_enabled": False, "ward_source": ward_source_diag}, warnings

    points_df = out.loc[valid_points].copy()
    points_df["orig_idx"] = points_df.index

    gpoints = gpd.GeoDataFrame(
        points_df,
        geometry=gpd.points_from_xy(pd.to_numeric(points_df["longitude"]), pd.to_numeric(points_df["latitude"])),
        crs="EPSG:4326",
    )
    gpoints_3435 = gpoints.to_crs("EPSG:3435")

    rows_assigned = 0
    rows_with_distance = 0

    for era in ["pre_2015", "2015_to_2023", "post_2023"]:
        mask = gpoints["ward_era"] == era
        if not mask.any():
            continue

        wards = era_ward_map.get(era)
        if wards is None or wards.empty:
            warnings.append(f"Ward geometry unavailable for era {era}; rows were left unassigned.")
            continue

        wards_3435 = wards.to_crs("EPSG:3435")

        subset = gpoints_3435.loc[mask].copy()
        subset_for_join = subset.drop(columns=["ward"], errors="ignore")
        joined = gpd.sjoin(subset_for_join, wards_3435, how="left", predicate="within")

        for rec in joined.itertuples(index=False):
            orig_idx = getattr(rec, "orig_idx")
            ward_value = getattr(rec, "ward", None)
            if pd.notna(ward_value):
                out.at[orig_idx, "ward"] = str(ward_value)
                rows_assigned += 1

        boundary_geoms = wards_3435.geometry.boundary
        if hasattr(boundary_geoms, "union_all"):
            boundary = boundary_geoms.union_all()
        else:
            boundary = boundary_geoms.unary_union
        if boundary is not None:
            dist_series = subset.geometry.distance(boundary)
            for orig_idx, dist in zip(subset["orig_idx"], dist_series):
                out.at[orig_idx, "dist_to_boundary_ft"] = float(dist)
                rows_with_distance += 1

    diag = {
        "ward_assignment_enabled": True,
        "ward_source": ward_source_diag,
        "rows_with_valid_points": int(valid_points.sum()),
        "rows_assigned_ward": int(pd.Series(out["ward"]).notna().sum()),
        "rows_with_distance": int(pd.Series(out["dist_to_boundary_ft"]).notna().sum()),
        "rows_assigned_counter": rows_assigned,
        "rows_distance_counter": rows_with_distance,
    }
    return out, diag, warnings


def save_scatter_map(df: pd.DataFrame, color_col: str, color_map: dict, title: str, out_path: str) -> str | None:
    if plt is None:
        return "matplotlib is not installed; map generation skipped"

    lat = pd.to_numeric(df.get("latitude"), errors="coerce")
    lon = pd.to_numeric(df.get("longitude"), errors="coerce")
    mask = lat.notna() & lon.notna()
    plot_df = df.loc[mask].copy()
    if plot_df.empty:
        return "No geocoded points available for plotting"

    values = plot_df[color_col].fillna("unknown")
    colors = [color_map.get(v, "#7f7f7f") for v in values]

    fig, ax = plt.subplots(figsize=(8, 8))
    ax.scatter(
        pd.to_numeric(plot_df["longitude"]),
        pd.to_numeric(plot_df["latitude"]),
        c=colors,
        s=8,
        alpha=0.7,
        linewidths=0,
    )
    ax.set_title(title)
    ax.set_xlabel("Longitude")
    ax.set_ylabel("Latitude")
    ax.grid(True, alpha=0.2)

    legend_labels = sorted(set(values.tolist()))
    handles = [
        plt.Line2D([0], [0], marker="o", color="w", label=label, markerfacecolor=color_map.get(label, "#7f7f7f"), markersize=6)
        for label in legend_labels
    ]
    ax.legend(handles=handles, loc="best", frameon=True)

    ensure_parent(out_path)
    fig.tight_layout()
    fig.savefig(out_path, dpi=180)
    plt.close(fig)
    return None


def build_enriched_dataframe(
    in_geocoded_csv: str,
    in_zoning_lookup_csv: str,
    in_ward_gpkg: str,
    in_pd_far_updates_csv: str | None = None,
    ward_layer_pre2015: str | None = None,
    ward_layer_2015: str | None = None,
    ward_layer_post2023: str | None = None,
) -> tuple[pd.DataFrame, dict, dict, list[str]]:
    df = pd.read_csv(in_geocoded_csv, dtype=str, low_memory=False)

    required_cols = ["from_zoning", "to_zoning", "matter_intro_date", "latitude", "longitude", "geocode_source"]
    missing = [col for col in required_cols if col not in df.columns]
    if missing:
        raise ValueError(f"Input geocoded file is missing required columns: {', '.join(missing)}")

    lookup, lookup_diag = load_far_lookup(in_zoning_lookup_csv)

    df = assign_far_columns(df, lookup)
    if in_pd_far_updates_csv:
        df = apply_pd_far_review_updates(df, in_pd_far_updates_csv)

    df, ward_diag, ward_warnings = assign_wards_and_distance(
        df=df,
        ward_gpkg_path=in_ward_gpkg,
        layer_pre2015=ward_layer_pre2015,
        layer_2015=ward_layer_2015,
        layer_post2023=ward_layer_post2023,
    )

    df = add_far_direction(df)
    return df, lookup_diag, ward_diag, ward_warnings


def build_sponsor_validation(df: pd.DataFrame) -> pd.DataFrame:
    sponsor_validation = pd.DataFrame(columns=["matter_id", "primary_sponsor_name", "primary_sponsor_ward", "ward", "ward_matches_sponsor"])
    if "primary_sponsor_ward" in df.columns:
        base_cols = ["matter_id", "primary_sponsor_ward", "ward"]
        if "primary_sponsor_name" in df.columns:
            base_cols.append("primary_sponsor_name")
        tmp = df[base_cols].copy()
        if "primary_sponsor_name" not in tmp.columns:
            tmp["primary_sponsor_name"] = None
        tmp["sponsor_ward_num"] = pd.to_numeric(tmp["primary_sponsor_ward"], errors="coerce")
        tmp["ward_num"] = pd.to_numeric(tmp["ward"], errors="coerce")
        tmp = tmp[tmp["sponsor_ward_num"].notna() & tmp["ward_num"].notna()].copy()
        tmp["ward_matches_sponsor"] = tmp["sponsor_ward_num"].astype(int) == tmp["ward_num"].astype(int)
        sponsor_validation = tmp[["matter_id", "primary_sponsor_name", "primary_sponsor_ward", "ward", "ward_matches_sponsor"]]
    return sponsor_validation


def build_ward_counts(df: pd.DataFrame) -> pd.DataFrame:
    return (
        df[df["ward"].notna()]
        .groupby("ward", as_index=False)
        .agg(rezoning_count=("matter_id", "count"))
        .sort_values("ward")
    )


def write_summary_json(
    out_summary_json: str,
    df: pd.DataFrame,
    sponsor_validation: pd.DataFrame,
    lookup_diag: dict,
    ward_diag: dict,
    warnings: list[str],
) -> None:
    far_change_num = pd.to_numeric(df["far_change"], errors="coerce")
    far_valid = far_change_num.notna()
    up_count = int((far_change_num > 0).sum())
    down_count = int((far_change_num < 0).sum())
    same_count = int((far_change_num == 0).sum())

    sponsor_match_rate = None
    if not sponsor_validation.empty:
        sponsor_match_rate = float(pd.to_numeric(sponsor_validation["ward_matches_sponsor"]).mean())

    summary = {
        "timestamp_utc": datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z"),
        "total_rows": int(len(df)),
        "geocode_source_counts": df["geocode_source"].fillna("unmatched").value_counts().to_dict(),
        "ward_assigned_rows": int(df["ward"].notna().sum()),
        "far_parse_from_success": int(df["from_far"].notna().sum()),
        "far_parse_to_success": int(df["to_far"].notna().sum()),
        "far_parse_both_success": int((df["from_far"].notna() & df["to_far"].notna()).sum()),
        "far_change_non_null": int(far_valid.sum()),
        "upzone_count": up_count,
        "downzone_count": down_count,
        "same_far_count": same_count,
        "upzone_share_with_far": (up_count / int(far_valid.sum())) if far_valid.sum() else None,
        "downzone_share_with_far": (down_count / int(far_valid.sum())) if far_valid.sum() else None,
        "same_far_share_with_far": (same_count / int(far_valid.sum())) if far_valid.sum() else None,
        "sponsor_validation_rows": int(len(sponsor_validation)),
        "sponsor_ward_match_rate": sponsor_match_rate,
        "lookup": lookup_diag,
        "ward_assignment": ward_diag,
        "warnings": warnings,
    }

    ensure_parent(out_summary_json)
    with open(out_summary_json, "w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2)


def main() -> int:
    args = parse_args()
    df, lookup_diag, ward_diag, ward_warnings = build_enriched_dataframe(
        in_geocoded_csv=args.in_geocoded_csv,
        in_zoning_lookup_csv=args.in_zoning_lookup_csv,
        in_ward_gpkg=args.in_ward_gpkg,
        ward_layer_pre2015=args.ward_layer_pre2015,
        ward_layer_2015=args.ward_layer_2015,
        ward_layer_post2023=args.ward_layer_post2023,
    )
    sponsor_validation = build_sponsor_validation(df)
    ward_counts = build_ward_counts(df)

    map_warnings = []
    source_map_warning = save_scatter_map(
        df,
        color_col="geocode_source",
        color_map={
            "parcel_match": "#1f77b4",
            "chicago_geocoder": "#2ca02c",
            "census_geocoder": "#ff7f0e",
            "unmatched": "#d62728",
            "unknown": "#7f7f7f",
        },
        title="Geocoded Rezonings by Source",
        out_path=args.out_map_source_pdf,
    )
    if source_map_warning:
        map_warnings.append(source_map_warning)

    upzone_map_warning = save_scatter_map(
        df,
        color_col="far_direction",
        color_map={
            "upzone": "#2ca02c",
            "downzone": "#d62728",
            "same": "#1f77b4",
            "unknown": "#7f7f7f",
        },
        title="Geocoded Rezonings by FAR Direction",
        out_path=args.out_map_upzone_pdf,
    )
    if upzone_map_warning:
        map_warnings.append(upzone_map_warning)

    ensure_parent(args.out_final_csv)
    ensure_parent(args.out_ward_counts_csv)
    ensure_parent(args.out_sponsor_validation_csv)

    df.to_csv(args.out_final_csv, index=False)
    ward_counts.to_csv(args.out_ward_counts_csv, index=False)
    sponsor_validation.to_csv(args.out_sponsor_validation_csv, index=False)

    if args.out_summary_json:
        write_summary_json(
            out_summary_json=args.out_summary_json,
            df=df,
            sponsor_validation=sponsor_validation,
            lookup_diag=lookup_diag,
            ward_diag=ward_diag,
            warnings=ward_warnings + map_warnings,
        )

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
