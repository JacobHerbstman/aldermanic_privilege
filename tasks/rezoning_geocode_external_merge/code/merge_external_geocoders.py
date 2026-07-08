import argparse
import csv
import json
import math
import os
import re
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

POINT_RE = re.compile(r"POINT\s*\(\s*([-0-9.]+)\s+([-0-9.]+)\s*\)", re.IGNORECASE)
PAIR_RE = re.compile(r"\(?\s*([-0-9.]+)\s*,\s*([-0-9.]+)\s*\)?")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-stage1-csv", required=True)
    parser.add_argument("--in-unmatched-csv", required=True)
    parser.add_argument("--in-chicago-results-csv")
    parser.add_argument("--in-census-results-csv")
    parser.add_argument("--in-chicago-upload-mapping-csv")
    parser.add_argument("--out-geocoded-csv", required=True)
    parser.add_argument("--out-remaining-unmatched-csv", required=True)
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


def parse_bool(value) -> bool:
    if pd.isna(value):
        return False
    if isinstance(value, bool):
        return value
    text = str(value).strip().lower()
    if text in {"1", "true", "t", "yes", "y"}:
        return True
    if text in {"0", "false", "f", "no", "n", ""}:
        return False
    return bool(value)


def normalize_id_series(series: pd.Series) -> pd.Series:
    out = series.astype(str).str.strip()
    return out.replace(
        {
            "": None,
            "nan": None,
            "NaN": None,
            "none": None,
            "None": None,
            "NA": None,
            "N/A": None,
        }
    )


def parse_lat_lon_text(value: str | None) -> tuple[float | None, float | None]:
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
        if first < 0 and second > 0 and -90 <= second <= 90 and -180 <= first <= 180:
            return second, first
        if -90 <= first <= 90 and -180 <= second <= 180:
            return first, second
        if -90 <= second <= 90 and -180 <= first <= 180:
            return second, first
    return None, None


def load_chicago_upload_mapping(path: str | None) -> tuple[pd.DataFrame, dict]:
    empty = pd.DataFrame(columns=["upload_row_number", "external_row_id"])
    if not path or not os.path.exists(path):
        return empty, {"file": path, "loaded": False, "reason": "file_not_found"}

    df = pd.read_csv(path, dtype=str, low_memory=False)
    if df.empty:
        return empty, {"file": path, "loaded": False, "reason": "empty_file"}

    cols = df.columns.tolist()
    row_col = choose_column(cols, ["upload_row_number", "row_number", "upload_row_num"], contains=["row", "number"])
    id_col = choose_column(cols, ["external_row_id", "matter_id", "row_id", "id", "unique_id"])
    include_col = choose_column(cols, ["upload_included", "is_uploadable", "include_in_upload"], contains=["included"])

    if row_col is None or id_col is None:
        return empty, {
            "file": path,
            "loaded": False,
            "reason": "required_columns_missing",
            "row_col": row_col,
            "id_col": id_col,
            "include_col": include_col,
        }

    out = pd.DataFrame()
    out["upload_row_number"] = pd.to_numeric(df[row_col], errors="coerce").astype("Int64")
    out["external_row_id"] = normalize_id_series(df[id_col])

    if include_col:
        include_mask = df[include_col].map(parse_bool)
        out = out[include_mask.values].copy()

    out = out[out["upload_row_number"].notna()]
    out = out[out["external_row_id"].notna()]
    out = out.sort_values("upload_row_number", kind="stable").drop_duplicates(subset=["upload_row_number"], keep="first")

    diag = {
        "file": path,
        "loaded": True,
        "rows_in_file": int(len(df)),
        "rows_usable": int(len(out)),
        "row_col": row_col,
        "id_col": id_col,
        "include_col": include_col,
    }
    return out, diag


def attach_row_order_ids(
    out_df: pd.DataFrame,
    mapping_df: pd.DataFrame,
    *,
    fill_only_missing: bool,
) -> tuple[pd.DataFrame, dict]:
    if mapping_df.empty:
        return out_df, {"applied": False, "reason": "mapping_empty"}

    out = out_df.copy()
    out["_upload_row_number"] = range(1, len(out) + 1)
    with_ids = out.merge(mapping_df, left_on="_upload_row_number", right_on="upload_row_number", how="left")
    mapped_rows = int(with_ids["external_row_id_y"].notna().sum())

    if "external_row_id_x" not in with_ids.columns:
        with_ids["external_row_id_x"] = None

    if fill_only_missing:
        base = normalize_id_series(with_ids["external_row_id_x"])
        with_ids["external_row_id"] = base.where(base.notna(), with_ids["external_row_id_y"])
    else:
        with_ids["external_row_id"] = with_ids["external_row_id_y"]

    with_ids = with_ids.drop(columns=["_upload_row_number", "upload_row_number", "external_row_id_x", "external_row_id_y"], errors="ignore")

    return with_ids, {
        "applied": True,
        "mapped_rows": mapped_rows,
        "fill_only_missing": bool(fill_only_missing),
    }


def load_chicago_results(path: str | None, mapping_path: str | None) -> tuple[pd.DataFrame, dict]:
    empty = pd.DataFrame(columns=["external_row_id", "latitude", "longitude", "provider"])
    mapping_df, mapping_diag = load_chicago_upload_mapping(mapping_path)

    if not path or not os.path.exists(path):
        return empty, {
            "file": path,
            "loaded": False,
            "reason": "file_not_found",
            "mapping": mapping_diag,
        }

    df = pd.read_csv(path, dtype=str, low_memory=False)
    if df.empty:
        return empty, {
            "file": path,
            "loaded": False,
            "reason": "empty_file",
            "mapping": mapping_diag,
        }

    cols = df.columns.tolist()
    id_col = choose_column(cols, ["external_row_id", "row_id", "id", "unique_id", "matter_id"])
    lat_col = choose_column(cols, ["latitude", "lat", "y", "ycoordinate", "y_coordinate"], contains=["lat"])
    lon_col = choose_column(cols, ["longitude", "lon", "lng", "x", "xcoordinate", "x_coordinate"], contains=["lon"])
    if lon_col is None:
        lon_col = choose_column(cols, ["x", "xcoordinate", "x_coordinate"])
    location_col = choose_column(cols, ["location", "point", "coordinates"])

    out = pd.DataFrame()
    if id_col:
        out["external_row_id"] = normalize_id_series(df[id_col])
    else:
        out["external_row_id"] = None

    out["latitude"] = df[lat_col].map(safe_float) if lat_col else None
    out["longitude"] = df[lon_col].map(safe_float) if lon_col else None

    if location_col:
        parsed = df[location_col].map(parse_lat_lon_text)
        parsed_lat = parsed.map(lambda p: p[0])
        parsed_lon = parsed.map(lambda p: p[1])
        out["latitude"] = out["latitude"].where(out["latitude"].notna(), parsed_lat)
        out["longitude"] = out["longitude"].where(out["longitude"].notna(), parsed_lon)

    mapping_attach_diag = {"applied": False, "reason": "not_needed"}
    missing_id_rows = out["external_row_id"].isna()

    if id_col is None:
        out, mapping_attach_diag = attach_row_order_ids(out, mapping_df, fill_only_missing=False)
    elif missing_id_rows.any() and not mapping_df.empty:
        out, mapping_attach_diag = attach_row_order_ids(out, mapping_df, fill_only_missing=True)

    out = out[out["external_row_id"].notna()]
    out["external_row_id"] = normalize_id_series(out["external_row_id"])
    out = out[out["external_row_id"].notna()]
    out = out[out["latitude"].between(-90, 90) & out["longitude"].between(-180, 180)]
    out = out.drop_duplicates(subset=["external_row_id"], keep="first")
    out["provider"] = "chicago_geocoder"

    diag = {
        "file": path,
        "loaded": True,
        "rows_in_file": int(len(df)),
        "rows_with_coords": int(len(out)),
        "id_col": id_col,
        "lat_col": lat_col,
        "lon_col": lon_col,
        "location_col": location_col,
        "mapping": mapping_diag,
        "mapping_attach": mapping_attach_diag,
    }
    return out, diag


def _looks_like_header(first_row: list[str]) -> bool:
    if not first_row:
        return False
    joined = " ".join(cell.lower() for cell in first_row)
    return "id" in joined or "address" in joined or "match" in joined


def load_census_results(path: str | None) -> tuple[pd.DataFrame, dict]:
    if not path or not os.path.exists(path):
        return pd.DataFrame(columns=["external_row_id", "latitude", "longitude", "provider"]), {
            "file": path,
            "loaded": False,
            "reason": "file_not_found",
        }

    with open(path, newline="", encoding="utf-8") as handle:
        reader = csv.reader(handle)
        try:
            first_row = next(reader)
        except StopIteration:
            first_row = []

    if not first_row:
        return pd.DataFrame(columns=["external_row_id", "latitude", "longitude", "provider"]), {
            "file": path,
            "loaded": False,
            "reason": "empty_file",
        }

    if _looks_like_header(first_row):
        df = pd.read_csv(path, dtype=str, low_memory=False)
    else:
        df = pd.read_csv(path, dtype=str, header=None, low_memory=False)
        expected = [
            "external_row_id",
            "input_address",
            "match_status",
            "match_type",
            "matched_address",
            "coordinates",
            "tiger_line_id",
            "tiger_line_side",
            "state_fips",
            "county_fips",
            "tract",
            "block",
        ]
        df = df.rename(columns={idx: name for idx, name in enumerate(expected)})

    cols = df.columns.tolist()
    id_col = choose_column(cols, ["external_row_id", "id", "unique_id"], contains=["id"])
    if id_col is None:
        id_col = cols[0]

    lat_col = choose_column(cols, ["latitude", "lat"], contains=["lat"])
    lon_col = choose_column(cols, ["longitude", "lon", "lng"], contains=["lon"])
    coord_col = choose_column(cols, ["coordinates", "matched_coordinates", "coordinate", "location"])

    out = pd.DataFrame()
    out["external_row_id"] = normalize_id_series(df[id_col])
    out["latitude"] = df[lat_col].map(safe_float) if lat_col else None
    out["longitude"] = df[lon_col].map(safe_float) if lon_col else None

    if coord_col:
        parsed = df[coord_col].map(parse_lat_lon_text)
        parsed_lat = parsed.map(lambda p: p[0])
        parsed_lon = parsed.map(lambda p: p[1])
        out["latitude"] = out["latitude"].where(out["latitude"].notna(), parsed_lat)
        out["longitude"] = out["longitude"].where(out["longitude"].notna(), parsed_lon)

    status_col = choose_column(cols, ["match", "match_status", "matched", "is_match"], contains=["match"])
    if status_col:
        status = df[status_col].fillna("").astype(str).str.strip().str.upper()
        keep = status.str.startswith("MATCH")
        out = out[keep.values]

    out = out[out["external_row_id"].notna()]
    out = out[out["latitude"].between(-90, 90) & out["longitude"].between(-180, 180)]
    out = out.drop_duplicates(subset=["external_row_id"], keep="first")
    out["provider"] = "census_geocoder"

    diag = {
        "file": path,
        "loaded": True,
        "rows_in_file": int(len(df)),
        "rows_with_coords": int(len(out)),
        "id_col": id_col,
        "lat_col": lat_col,
        "lon_col": lon_col,
        "coord_col": coord_col,
        "status_col": status_col,
    }
    return out, diag


def main() -> int:
    args = parse_args()

    stage1_df = pd.read_csv(args.in_stage1_csv, dtype=str, low_memory=False)
    unmatched_df = pd.read_csv(args.in_unmatched_csv, dtype=str, low_memory=False)

    for required in ["external_row_id", "geocode_source", "geocode_confidence", "latitude", "longitude"]:
        if required not in stage1_df.columns:
            raise ValueError(f"Stage1 file is missing required column: {required}")

    chicago_df, chicago_diag = load_chicago_results(
        args.in_chicago_results_csv,
        args.in_chicago_upload_mapping_csv,
    )
    census_df, census_diag = load_census_results(args.in_census_results_csv)

    chicago_map = chicago_df.set_index("external_row_id").to_dict("index") if not chicago_df.empty else {}
    census_map = census_df.set_index("external_row_id").to_dict("index") if not census_df.empty else {}

    unmatched_ids = set(unmatched_df.get("external_row_id", pd.Series(dtype=str)).astype(str))

    chicago_added = 0
    census_added = 0

    for idx, row in stage1_df.iterrows():
        row_id = str(row.get("external_row_id", ""))
        if not row_id or row_id not in unmatched_ids:
            continue

        source = str(row.get("geocode_source", "")).strip().lower()
        lat = safe_float(row.get("latitude"))
        lon = safe_float(row.get("longitude"))
        if source == "parcel_match" and lat is not None and lon is not None:
            continue

        update = None
        provider = None
        if row_id in chicago_map:
            update = chicago_map[row_id]
            provider = "chicago_geocoder"
        elif row_id in census_map:
            update = census_map[row_id]
            provider = "census_geocoder"

        if update is None:
            continue

        stage1_df.at[idx, "latitude"] = update["latitude"]
        stage1_df.at[idx, "longitude"] = update["longitude"]
        stage1_df.at[idx, "geocode_source"] = provider
        stage1_df.at[idx, "geocode_confidence"] = "external_geocoder"
        stage1_df.at[idx, "matched_pin"] = row.get("matched_pin") if "matched_pin" in stage1_df.columns else None

        if provider == "chicago_geocoder":
            chicago_added += 1
        elif provider == "census_geocoder":
            census_added += 1

    remaining_unmatched = stage1_df[
        stage1_df["latitude"].isna() | stage1_df["longitude"].isna() | (stage1_df["geocode_source"] == "unmatched")
    ].copy()

    ensure_parent(args.out_geocoded_csv)
    ensure_parent(args.out_remaining_unmatched_csv)
    if args.out_summary_json:
        ensure_parent(args.out_summary_json)

    stage1_df.to_csv(args.out_geocoded_csv, index=False)
    remaining_unmatched.to_csv(args.out_remaining_unmatched_csv, index=False)

    source_counts = stage1_df["geocode_source"].fillna("unmatched").value_counts().to_dict()
    total_rows = int(len(stage1_df))
    matched_rows = int(total_rows - len(remaining_unmatched))

    if args.out_summary_json:
        summary = {
            "timestamp_utc": datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z"),
            "total_rows": total_rows,
            "matched_rows": matched_rows,
            "remaining_unmatched_rows": int(len(remaining_unmatched)),
            "source_counts": source_counts,
            "chicago_geocoder_rows_added": chicago_added,
            "census_geocoder_rows_added": census_added,
            "chicago_results": chicago_diag,
            "census_results": census_diag,
        }
        with open(args.out_summary_json, "w", encoding="utf-8") as handle:
            json.dump(summary, handle, indent=2)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
