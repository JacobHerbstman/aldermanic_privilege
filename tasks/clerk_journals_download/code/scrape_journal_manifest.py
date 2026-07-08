import argparse
import json
import re
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import urljoin

import pandas as pd
import requests
from bs4 import BeautifulSoup

from journal_parsers import (
    assign_collision_suffixes,
    build_base_filename,
    dedupe_manifest,
    fallback_basename,
    normalize_spaces,
    parse_meeting_date_iso,
    parse_meeting_type,
)


BASE_URL = "https://www.chicityclerk.com/legislation-records/journals-and-reports/journals-proceedings"
YEAR_PATTERN = re.compile(r"(19|20)\d{2}")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--years-csv")
    parser.add_argument("--year-start", type=int)
    parser.add_argument("--year-end", type=int)
    parser.add_argument("--timeout-seconds", type=float, default=60.0)
    parser.add_argument("--out-manifest-csv", required=True)
    parser.add_argument("--out-scrape-report-json")
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def parse_requested_years(args: argparse.Namespace) -> list[int]:
    if args.years_csv:
        years = [int(part.strip()) for part in args.years_csv.split(",") if part.strip()]
        if not years:
            raise ValueError("Expected at least one year in --years-csv.")
        return sorted(set(years))

    if args.year_start is None or args.year_end is None:
        raise ValueError("Provide --years-csv or both --year-start and --year-end.")
    if args.year_start > args.year_end:
        raise ValueError("--year-start must be less than or equal to --year-end.")

    return list(range(args.year_start, args.year_end + 1))


def discover_year_filters(
    session: requests.Session,
    timeout_seconds: float,
) -> dict[int, str]:
    response = session.get(BASE_URL, timeout=timeout_seconds)
    response.raise_for_status()

    soup = BeautifulSoup(response.text, "html.parser")
    select = soup.find("select", attrs={"name": "field_year_value"})
    if select is None:
        raise RuntimeError("Could not find year dropdown (field_year_value).")

    mapping: dict[int, str] = {}
    for option in select.find_all("option"):
        value = normalize_spaces(option.get("value"))
        label = normalize_spaces(option.get_text(" ", strip=True))
        if not value:
            continue
        match = YEAR_PATTERN.search(label)
        if not match:
            continue
        year = int(match.group(0))
        if year not in mapping:
            mapping[year] = value
    return mapping


def extract_year_rows(
    session: requests.Session,
    year: int,
    filter_value: str,
    timeout_seconds: float,
) -> list[dict]:
    response = session.get(
        BASE_URL,
        params={"field_year_value": filter_value},
        timeout=timeout_seconds,
    )
    response.raise_for_status()

    soup = BeautifulSoup(response.text, "html.parser")
    table_rows = soup.select("table tbody tr")
    if not table_rows:
        table_rows = soup.find_all("tr")

    rows: list[dict] = []
    for tr in table_rows:
        pdf_url = None
        for link in tr.find_all("a", href=True):
            href = normalize_spaces(link.get("href"))
            if ".pdf" in href.lower():
                pdf_url = urljoin(BASE_URL, href)
                break
        if not pdf_url:
            continue

        row_text = normalize_spaces(tr.get_text(" ", strip=True))
        if "journal" not in row_text.lower() and "proceedings" not in row_text.lower():
            continue

        title = None
        for cell in tr.find_all("td"):
            cell_text = normalize_spaces(cell.get_text(" ", strip=True))
            if "journal" in cell_text.lower() and "proceedings" in cell_text.lower():
                title = cell_text
                break
        if not title:
            title = row_text

        meeting_date = parse_meeting_date_iso(title)
        meeting_type_raw, meeting_type_norm = parse_meeting_type(title)
        fallback_name = fallback_basename(pdf_url, title)
        filename = build_base_filename(meeting_date, meeting_type_norm, fallback_name)

        rows.append(
            {
                "year": year,
                "year_filter_value": filter_value,
                "meeting_date": meeting_date,
                "meeting_type_raw": meeting_type_raw,
                "meeting_type_norm": meeting_type_norm,
                "document_title": title,
                "pdf_url": pdf_url,
                "filename": filename,
                "rel_local_path": f"{year}/{filename}",
            }
        )

    return rows


def add_collision_suffixes(manifest: pd.DataFrame) -> pd.DataFrame:
    if manifest.empty:
        return manifest

    out = manifest.copy()
    out = out.sort_values(
        ["year", "meeting_date", "document_title", "pdf_url"],
        kind="stable",
        na_position="last",
    ).reset_index(drop=True)

    for year in out["year"].dropna().unique().tolist():
        mask = out["year"] == year
        names = out.loc[mask, "filename"].tolist()
        out.loc[mask, "filename"] = assign_collision_suffixes(names)

    out["rel_local_path"] = out["year"].astype(int).astype(str) + "/" + out["filename"]
    return out


def main() -> int:
    args = parse_args()
    ensure_parent(args.out_manifest_csv)
    if args.out_scrape_report_json:
        ensure_parent(args.out_scrape_report_json)

    requested_years = parse_requested_years(args)
    session = requests.Session()
    year_mapping = discover_year_filters(session=session, timeout_seconds=args.timeout_seconds)

    missing_years = [year for year in requested_years if year not in year_mapping]
    scrape_failures: dict[int, str] = {}
    all_rows: list[dict] = []
    raw_counts_by_year: dict[int, int] = {}

    for year in requested_years:
        if year in missing_years:
            raw_counts_by_year[year] = 0
            continue
        try:
            rows = extract_year_rows(
                session=session,
                year=year,
                filter_value=year_mapping[year],
                timeout_seconds=args.timeout_seconds,
            )
            raw_counts_by_year[year] = len(rows)
            all_rows.extend(rows)
        except Exception as exc:  # noqa: BLE001
            scrape_failures[year] = str(exc)
            raw_counts_by_year[year] = 0

    manifest = pd.DataFrame(all_rows)
    if manifest.empty:
        manifest = pd.DataFrame(
            columns=[
                "year",
                "year_filter_value",
                "meeting_date",
                "meeting_type_raw",
                "meeting_type_norm",
                "document_title",
                "pdf_url",
                "filename",
                "rel_local_path",
            ]
        )

    before_dedupe = len(manifest)
    manifest = dedupe_manifest(manifest)
    after_dedupe = len(manifest)
    manifest = add_collision_suffixes(manifest)

    manifest.to_csv(args.out_manifest_csv, index=False)

    final_counts = (
        manifest.groupby("year").size().astype(int).to_dict()
        if not manifest.empty
        else {year: 0 for year in requested_years}
    )

    if args.out_scrape_report_json:
        report = {
            "generated_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z"),
            "base_url": BASE_URL,
            "requested_years": requested_years,
            "year_options_discovered": len(year_mapping),
            "missing_years": missing_years,
            "scrape_failures": {str(key): value for key, value in scrape_failures.items()},
            "rows_scraped_by_year_raw": {str(key): int(value) for key, value in raw_counts_by_year.items()},
            "rows_by_year_final": {str(int(key)): int(value) for key, value in final_counts.items()},
            "manifest_rows_before_dedupe": int(before_dedupe),
            "manifest_rows_after_dedupe": int(after_dedupe),
            "duplicates_dropped": int(before_dedupe - after_dedupe),
        }

        with Path(args.out_scrape_report_json).open("w", encoding="utf-8") as handle:
            json.dump(report, handle, indent=2)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
