import argparse
import hashlib
import json
import math
import re
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

POINT_RE = re.compile(r"POINT\s*\(\s*([-0-9.]+)\s+([-0-9.]+)\s*\)", re.IGNORECASE)
PAIR_RE = re.compile(r"\(?\s*([-0-9.]+)\s*[,;]\s*([-0-9.]+)\s*\)?")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-xlsx", required=True)
    parser.add_argument("--out-csv", required=True)
    parser.add_argument("--out-meta-json")
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


def parse_lat_lon_text(value: str | None) -> tuple[float | None, float | None]:
    if value is None or pd.isna(value):
        return None, None
    text = str(value).strip()
    if not text or text in {"---", "nan", "None"}:
        return None, None

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


def file_sha256(path: str) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def main() -> int:
    args = parse_args()

    df = pd.read_excel(args.in_xlsx, dtype=str)
    cols = df.columns.tolist()

    row_col = choose_column(cols, ["ROW_NUMBER", "row_number", "rownum"], contains=["row", "number"])
    address_col = choose_column(cols, ["address", "input_address"], contains=["address"])
    full_address_col = choose_column(cols, ["full address", "full_address", "matched_address"], contains=["full", "address"])
    status_col = choose_column(cols, ["status", "match_status"], contains=["status"])
    lat_col = choose_column(cols, ["latitude", "lat"], contains=["lat"])
    lon_col = choose_column(cols, ["longitude", "lon", "lng"], contains=["lon"])
    latlon_col = choose_column(cols, ["lat/long coordinates", "coordinates", "location", "point"], contains=["coord"])

    out = pd.DataFrame()
    if row_col:
        out["upload_row_number"] = pd.to_numeric(df[row_col], errors="coerce").astype("Int64")
    else:
        out["upload_row_number"] = range(1, len(df) + 1)

    out["address"] = df[address_col].astype(str) if address_col else None
    out["matched_address"] = df[full_address_col].astype(str) if full_address_col else None
    out["status"] = df[status_col].astype(str) if status_col else None

    out["latitude"] = df[lat_col].map(safe_float) if lat_col else None
    out["longitude"] = df[lon_col].map(safe_float) if lon_col else None

    if latlon_col:
        parsed = df[latlon_col].map(parse_lat_lon_text)
        parsed_lat = parsed.map(lambda p: p[0])
        parsed_lon = parsed.map(lambda p: p[1])
        out["latitude"] = out["latitude"].where(out["latitude"].notna(), parsed_lat)
        out["longitude"] = out["longitude"].where(out["longitude"].notna(), parsed_lon)
        out["lat_long_coordinates"] = df[latlon_col].astype(str)
    else:
        out["lat_long_coordinates"] = None

    out = out.sort_values("upload_row_number", kind="stable")

    ensure_parent(args.out_csv)
    out.to_csv(args.out_csv, index=False)

    if args.out_meta_json:
        ensure_parent(args.out_meta_json)
        summary = {
            "timestamp_utc": datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z"),
            "source_xlsx": args.in_xlsx,
            "source_xlsx_sha256": file_sha256(args.in_xlsx),
            "rows_in_xlsx": int(len(df)),
            "rows_exported": int(len(out)),
            "rows_with_valid_coords": int((out["latitude"].between(-90, 90) & out["longitude"].between(-180, 180)).sum()),
            "detected_columns": {
                "row_col": row_col,
                "address_col": address_col,
                "full_address_col": full_address_col,
                "status_col": status_col,
                "lat_col": lat_col,
                "lon_col": lon_col,
                "latlon_col": latlon_col,
            },
            "output_csv": args.out_csv,
        }
        with open(args.out_meta_json, "w", encoding="utf-8") as handle:
            json.dump(summary, handle, indent=2)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
