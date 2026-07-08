import argparse
import re
import time
from datetime import date, datetime
from pathlib import Path

import pandas as pd
import requests

BASE_URL = "https://api.chicityclerkelms.chicago.gov/matter"
KEYWORDS = {
    "flag_keyword_reclassification": "zoning reclassification",
    "flag_keyword_planned_development": "planned development",
    "flag_keyword_map_amendment": "zoning map amendment",
    "flag_keyword_lakefront": "lakefront protection",
}
RECLASS_TITLE_PATTERNS = [
    re.compile(r"\bzoning\s+reclass\w*\b.*\bmap\b.*\bno\b", re.IGNORECASE),
    re.compile(r"\bzoning\s+reclass\w*\b\s+on\s+map\b", re.IGNORECASE),
    re.compile(r"\breclassification\b.*\bmap\b.*\bno\b", re.IGNORECASE),
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--start-date", required=True)
    parser.add_argument("--end-date", required=True)
    parser.add_argument("--top", type=int, default=500)
    parser.add_argument("--sleep-seconds", type=float, default=0.2)
    parser.add_argument("--out-matters-csv", required=True)
    parser.add_argument("--out-seed-csv", required=True)
    return parser.parse_args()


def parse_cli_date(value: str) -> date:
    return datetime.strptime(value, "%Y-%m-%d").date()


def parse_elms_datetime(value) -> datetime | None:
    if value is None:
        return None
    if isinstance(value, datetime):
        return value

    text = str(value).strip()
    if not text:
        return None

    try:
        return datetime.fromisoformat(text.replace("Z", "+00:00")).replace(tzinfo=None)
    except ValueError:
        return None


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def normalize_matter_id(value) -> str | None:
    if value is None or pd.isna(value):
        return None
    text = str(value).strip()
    if not text:
        return None
    return text


def build_file_year_filter(file_year: int) -> str:
    return (
        "type eq 'Ordinance' and "
        "(supersededBy eq null or supersededBy eq '') and "
        f"fileYear eq {file_year}"
    )


def fetch_file_year_ordinances(
    session: requests.Session,
    file_year: int,
    top: int,
    sleep_seconds: float,
) -> tuple[list[dict], list[int], int | None]:
    if top <= 0:
        raise ValueError("`top` must be positive.")
    page_size = min(top, 500)

    records: list[dict] = []
    page_sizes: list[int] = []
    skip = 0
    total_count: int | None = None

    while True:
        params = {
            "filter": build_file_year_filter(file_year),
            "top": page_size,
            "skip": skip,
            "sort": "recordNumber asc",
        }
        response = session.get(BASE_URL, params=params, timeout=90)
        response.raise_for_status()

        payload = response.json()
        if not isinstance(payload, dict):
            raise RuntimeError("Expected a JSON object from ELMS /matter endpoint.")

        page_data = payload.get("data", [])
        if not isinstance(page_data, list):
            raise RuntimeError("Expected `data` list from ELMS /matter endpoint.")

        meta = payload.get("meta", {}) if isinstance(payload.get("meta"), dict) else {}
        if total_count is None:
            try:
                total_count = int(meta.get("count")) if meta.get("count") is not None else None
            except (TypeError, ValueError):
                total_count = None

        current_size = len(page_data)
        page_sizes.append(current_size)
        records.extend(page_data)

        if current_size < page_size:
            break

        skip += page_size
        if sleep_seconds > 0:
            time.sleep(sleep_seconds)

    return records, page_sizes, total_count


def coerce_matter_row(raw: dict) -> dict:
    intro_raw = parse_elms_datetime(raw.get("introductionDate"))
    final_raw = parse_elms_datetime(raw.get("finalActionDate"))

    date_imputed = intro_raw is None and final_raw is not None
    intro_for_window = intro_raw if intro_raw is not None else final_raw

    row = dict(raw)
    row["matter_id"] = normalize_matter_id(raw.get("recordNumber"))
    row["matter_guid"] = normalize_matter_id(raw.get("matterId"))
    row["matter_file"] = normalize_matter_id(raw.get("recordNumber"))
    row["matter_title"] = raw.get("title")
    row["matter_type_name"] = raw.get("type")
    row["matter_status_name"] = raw.get("status")
    row["matter_body_name"] = raw.get("controllingBody")
    row["matter_intro_date_raw"] = intro_raw.date().isoformat() if intro_raw else None
    row["matter_intro_date"] = intro_for_window.date().isoformat() if intro_for_window else None
    row["matter_passed_date"] = final_raw.date().isoformat() if final_raw else None
    row["matter_enactment_number"] = raw.get("recordNumber")
    row["date_imputed_from_final_action"] = bool(date_imputed)
    row["source_system"] = "elms"
    return row


def has_reclass_title(value) -> bool:
    if value is None or pd.isna(value):
        return False
    text = str(value).replace("\u2013", "-").replace("\u2014", "-")
    text = re.sub(r"[,:;.]", " ", text)
    text = re.sub(r"\s+", " ", text).strip()
    return any(pattern.search(text) for pattern in RECLASS_TITLE_PATTERNS)


def make_seed_table(matters: pd.DataFrame) -> pd.DataFrame:
    if matters.empty:
        return pd.DataFrame(
            columns=[
                "matter_id",
                "matter_file",
                "matter_title",
                "matter_body_name",
                *KEYWORDS.keys(),
                "flag_title_map_reclassification",
                "flag_body_zoning_committee",
                "flag_title_or_body_zoning",
                "seed_is_candidate",
                "zoning_class",
            ]
        )

    title = matters["matter_title"].fillna("").str.lower()
    body = matters["matter_body_name"].fillna("").str.lower()

    out = matters[["matter_id", "matter_file", "matter_title", "matter_body_name"]].copy()
    for flag, phrase in KEYWORDS.items():
        out[flag] = title.str.contains(re.escape(phrase), regex=True)

    out["flag_title_map_reclassification"] = matters["matter_title"].map(has_reclass_title)
    out["flag_body_zoning_committee"] = body.str.contains("committee on zoning")
    out["flag_title_or_body_zoning"] = title.str.contains("zoning") | body.str.contains("zoning")
    out["seed_is_candidate"] = (
        out[list(KEYWORDS.keys())].any(axis=1)
        | out["flag_title_map_reclassification"]
        | out["flag_body_zoning_committee"]
        | out["flag_title_or_body_zoning"]
    )

    out["zoning_class"] = None
    out.loc[out["flag_title_map_reclassification"] | out["flag_keyword_reclassification"], "zoning_class"] = (
        "reclassification"
    )
    out.loc[
        out["zoning_class"].isna() & out["flag_keyword_planned_development"],
        "zoning_class",
    ] = "planned_development"
    out.loc[out["zoning_class"].isna() & out["seed_is_candidate"], "zoning_class"] = (
        "other_zoning_candidate"
    )

    return out


def main() -> int:
    args = parse_args()
    start_date = parse_cli_date(args.start_date)
    end_date = parse_cli_date(args.end_date)

    ensure_parent(args.out_matters_csv)
    ensure_parent(args.out_seed_csv)

    session = requests.Session()

    raw_rows: list[dict] = []
    for file_year in range(start_date.year, end_date.year + 1):
        rows, _, _ = fetch_file_year_ordinances(
            session=session,
            file_year=file_year,
            top=args.top,
            sleep_seconds=args.sleep_seconds,
        )
        raw_rows.extend(rows)

    coerced = [coerce_matter_row(row) for row in raw_rows]

    filtered = []
    dropped_for_missing_intro_after_fallback = 0
    for row in coerced:
        intro_text = row.get("matter_intro_date")
        if not intro_text:
            dropped_for_missing_intro_after_fallback += 1
            continue
        intro_date = parse_cli_date(intro_text)
        if start_date <= intro_date <= end_date:
            filtered.append(row)

    matters = pd.DataFrame(filtered)
    if matters.empty:
        matters = pd.DataFrame(
            columns=[
                "matter_id",
                "matter_guid",
                "matter_file",
                "matter_title",
                "matter_type_name",
                "matter_status_name",
                "matter_body_name",
                "matter_intro_date_raw",
                "matter_intro_date",
                "matter_passed_date",
                "matter_enactment_number",
                "date_imputed_from_final_action",
                "source_system",
            ]
        )

    matters["matter_id"] = matters["matter_id"].map(normalize_matter_id)
    matters["matter_guid"] = matters["matter_guid"].map(normalize_matter_id)
    matters = matters[matters["matter_id"].notna()].copy()
    matters = matters.sort_values(["matter_intro_date", "matter_id"], na_position="last").reset_index(drop=True)
    if not matters.empty and int(matters["matter_id"].duplicated().sum()) > 0:
        matters = matters.drop_duplicates(subset=["matter_id"], keep="first").reset_index(drop=True)

    seed = make_seed_table(matters)

    matters.to_csv(args.out_matters_csv, index=False)
    seed.to_csv(args.out_seed_csv, index=False)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
