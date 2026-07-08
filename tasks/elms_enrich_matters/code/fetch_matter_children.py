import argparse
import hashlib
import json
import re
import time
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import quote

import pandas as pd
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

BASE_URL = "https://api.chicityclerkelms.chicago.gov"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-matters-csv", required=True)
    parser.add_argument("--in-seed-csv", required=True)
    parser.add_argument("--sleep-seconds", type=float, default=0.2)
    parser.add_argument("--max-retries", type=int, default=4)
    parser.add_argument("--timeout-seconds", type=int, default=60)
    parser.add_argument("--out-sponsors", required=True)
    parser.add_argument("--out-histories", required=True)
    parser.add_argument("--out-attachments", required=True)
    parser.add_argument("--out-indexes")
    parser.add_argument("--out-persons")
    parser.add_argument("--out-candidates", required=True)
    parser.add_argument("--out-report-json")
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def normalize_matter_id(value) -> str | None:
    if value is None or pd.isna(value):
        return None
    text = str(value).strip()
    if not text:
        return None
    return text


def normalize_text(value) -> str | None:
    if value is None or pd.isna(value):
        return None
    text = str(value).strip()
    return text or None


def first_value(row: dict, keys: list[str]):
    for key in keys:
        if key in row and row[key] not in (None, ""):
            return row[key]
    return None


def make_session(max_retries: int) -> requests.Session:
    session = requests.Session()
    retry = Retry(
        total=max_retries,
        connect=max_retries,
        read=max_retries,
        status=max_retries,
        status_forcelist=[429, 500, 502, 503, 504],
        backoff_factor=0.5,
        allowed_methods=["GET"],
    )
    adapter = HTTPAdapter(max_retries=retry)
    session.mount("https://", adapter)
    session.mount("http://", adapter)
    return session


def fetch_json(session: requests.Session, url: str, timeout_seconds: int):
    response = session.get(url, timeout=timeout_seconds)
    response.raise_for_status()
    payload = response.json()
    if not isinstance(payload, dict):
        raise RuntimeError(f"Expected JSON object from ELMS endpoint: {url}")
    return payload


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


def to_bool_series(series: pd.Series) -> pd.Series:
    return series.map(parse_bool)


def normalize_attachment_id(path: str | None, file_name: str | None, idx: int) -> str:
    token = "|".join(
        [
            str(path or "").strip(),
            str(file_name or "").strip(),
            str(idx),
        ]
    )
    digest = hashlib.sha1(token.encode("utf-8")).hexdigest()[:16]
    return f"elms_{digest}"


def extract_sponsor_ward(office: str | None, sponsor_name: str | None) -> str | None:
    for raw in [office, sponsor_name]:
        text = normalize_text(raw)
        if not text:
            continue
        match = re.search(r"\b(\d{1,2})(?:ST|ND|RD|TH)?\s+WARD\b", text, flags=re.IGNORECASE)
        if match:
            return match.group(1)
    return None


def normalize_sponsor(matter_id: str, row: dict, seq: int) -> dict:
    out = dict(row)
    sponsor_name = normalize_text(first_value(row, ["sponsorName", "name", "fullName"]))
    sponsor_type = normalize_text(first_value(row, ["sponsorType", "type"]))
    office = normalize_text(first_value(row, ["office", "officeName"]))

    out["matter_id"] = matter_id
    out["person_id"] = normalize_text(first_value(row, ["personId", "id"]))
    out["person_full_name"] = sponsor_name
    out["sponsor_sequence"] = seq
    out["sponsor_ward"] = extract_sponsor_ward(office, sponsor_name)
    out["sponsor_type"] = sponsor_type
    out["sponsor_office"] = office
    return out


def normalize_history(matter_id: str, seq: int, row: dict) -> dict:
    out = dict(row)
    out["matter_id"] = matter_id
    out["action_seq"] = first_value(row, ["sort", "historyId"]) or seq
    out["action_date"] = first_value(row, ["actionDate"])
    out["action_name"] = first_value(row, ["actionName", "name"])
    out["action_body"] = first_value(row, ["actionByName", "meetingName"])
    return out


def normalize_attachment(matter_id: str, idx: int, row: dict) -> dict:
    out = dict(row)
    file_name = normalize_text(first_value(row, ["fileName", "name"]))
    path = normalize_text(first_value(row, ["path", "url"]))

    out["matter_id"] = matter_id
    out["attachment_id"] = normalize_attachment_id(path=path, file_name=file_name, idx=idx)
    out["attachment_name"] = file_name
    out["attachment_url"] = path
    out["attachment_type"] = normalize_text(first_value(row, ["attachmentType", "type"]))
    return out


def build_candidates(seed: pd.DataFrame) -> pd.DataFrame:
    candidates = seed.copy()
    if "matter_id" not in candidates.columns and "MatterId" in candidates.columns:
        candidates["matter_id"] = candidates["MatterId"]

    candidates["matter_id"] = candidates["matter_id"].map(normalize_matter_id)

    expected_flags = [
        "flag_keyword_reclassification",
        "flag_keyword_planned_development",
        "flag_keyword_map_amendment",
        "flag_keyword_lakefront",
        "flag_title_map_reclassification",
        "flag_body_zoning_committee",
        "flag_title_or_body_zoning",
        "seed_is_candidate",
    ]
    for col in expected_flags:
        if col not in candidates.columns:
            candidates[col] = False
        candidates[col] = to_bool_series(candidates[col])

    candidates["flag_index_reclassification"] = False
    candidates["flag_index_zoning"] = False

    candidates["is_final_candidate"] = (
        candidates["seed_is_candidate"]
        | candidates["flag_index_reclassification"]
        | candidates["flag_index_zoning"]
    )

    if "zoning_class" not in candidates.columns:
        candidates["zoning_class"] = None

    candidates.loc[
        candidates["flag_title_map_reclassification"] | candidates["flag_keyword_reclassification"],
        "zoning_class",
    ] = "reclassification"
    candidates.loc[
        candidates["zoning_class"].isna() & candidates["flag_keyword_planned_development"],
        "zoning_class",
    ] = "planned_development"
    candidates.loc[
        candidates["zoning_class"].isna() & candidates["is_final_candidate"],
        "zoning_class",
    ] = "other_zoning_candidate"

    candidates = candidates[candidates["matter_id"].notna()].copy()
    candidates = candidates.sort_values("matter_id", na_position="last").reset_index(drop=True)
    return candidates


def safe_ratio(numer: int, denom: int) -> float | None:
    if denom == 0:
        return None
    return float(numer) / float(denom)


def ensure_columns(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    out = df.copy()
    for col in columns:
        if col not in out.columns:
            out[col] = None
    return out[columns + [c for c in out.columns if c not in columns]]


def unique_matter_ids(df: pd.DataFrame) -> set[str]:
    if "matter_id" not in df.columns or df.empty:
        return set()
    return {
        value
        for value in (normalize_matter_id(v) for v in df["matter_id"].tolist())
        if value is not None
    }


def detail_url_for_record_number(record_number: str) -> str:
    encoded = quote(record_number, safe="")
    return f"{BASE_URL}/matter/recordNumber/{encoded}"


def detail_url_for_guid(matter_guid: str) -> str:
    encoded = quote(matter_guid, safe="")
    return f"{BASE_URL}/matter/{encoded}"


def run_pipeline(args: argparse.Namespace) -> int:
    output_paths = [
        args.out_sponsors,
        args.out_histories,
        args.out_attachments,
        args.out_candidates,
    ]
    if args.out_report_json:
        output_paths.append(args.out_report_json)
    if args.out_indexes:
        output_paths.append(args.out_indexes)
    if args.out_persons:
        output_paths.append(args.out_persons)

    for path in output_paths:
        ensure_parent(path)

    matters = pd.read_csv(args.in_matters_csv, dtype=str, low_memory=False)
    seed = pd.read_csv(args.in_seed_csv, dtype=str, low_memory=False)

    if "matter_id" not in seed.columns and "MatterId" in seed.columns:
        seed["matter_id"] = seed["MatterId"]

    seed["matter_id"] = seed["matter_id"].map(normalize_matter_id)

    if "seed_is_candidate" not in seed.columns:
        seed["seed_is_candidate"] = True
    else:
        seed["seed_is_candidate"] = to_bool_series(seed["seed_is_candidate"])

    matter_lookup = matters[[c for c in ["matter_id", "matter_guid"] if c in matters.columns]].copy()
    if "matter_id" not in matter_lookup.columns:
        matter_lookup["matter_id"] = None
    if "matter_guid" not in matter_lookup.columns:
        matter_lookup["matter_guid"] = None
    matter_lookup["matter_id"] = matter_lookup["matter_id"].map(normalize_matter_id)
    matter_lookup["matter_guid"] = matter_lookup["matter_guid"].map(normalize_matter_id)
    matter_guid_by_id = {
        row["matter_id"]: row["matter_guid"]
        for _, row in matter_lookup.dropna(subset=["matter_id"]).iterrows()
    }

    seed_ids = (
        seed.loc[seed["seed_is_candidate"].fillna(False), "matter_id"].dropna().drop_duplicates().tolist()
    )

    session = make_session(args.max_retries)

    sponsors_rows: list[dict] = []
    histories_rows: list[dict] = []
    attachments_rows: list[dict] = []
    indexes_rows: list[dict] = []
    request_errors: list[dict] = []
    resolved_by_record_number = 0
    resolved_by_guid = 0

    for matter_id in seed_ids:
        detail = None
        detail_error = None
        try:
            detail = fetch_json(session, detail_url_for_record_number(matter_id), args.timeout_seconds)
            resolved_by_record_number += 1
        except Exception as exc:  # noqa: BLE001
            detail_error = str(exc)

        if detail is None:
            matter_guid = matter_guid_by_id.get(matter_id)
            if matter_guid:
                try:
                    detail = fetch_json(session, detail_url_for_guid(matter_guid), args.timeout_seconds)
                    resolved_by_guid += 1
                except Exception as exc:  # noqa: BLE001
                    detail_error = str(exc)

        if detail is None:
            request_errors.append(
                {
                    "matter_id": matter_id,
                    "endpoint": "matter_detail",
                    "error": detail_error or "Unknown detail fetch error",
                }
            )
            if args.sleep_seconds > 0:
                time.sleep(args.sleep_seconds)
            continue

        for seq, row in enumerate(detail.get("sponsors") or [], start=1):
            if isinstance(row, dict):
                sponsors_rows.append(normalize_sponsor(matter_id, row, seq))

        for seq, row in enumerate(detail.get("actions") or [], start=1):
            if isinstance(row, dict):
                histories_rows.append(normalize_history(matter_id, seq, row))

        for seq, row in enumerate(detail.get("attachments") or [], start=1):
            if isinstance(row, dict):
                attachments_rows.append(normalize_attachment(matter_id, seq, row))

        if args.sleep_seconds > 0:
            time.sleep(args.sleep_seconds)

    sponsors = pd.DataFrame(sponsors_rows)
    histories = pd.DataFrame(histories_rows)
    attachments = pd.DataFrame(attachments_rows)
    indexes = pd.DataFrame(indexes_rows)

    sponsors = ensure_columns(
        sponsors,
        ["matter_id", "person_id", "person_full_name", "sponsor_sequence", "sponsor_ward"],
    )
    histories = ensure_columns(
        histories,
        ["matter_id", "action_seq", "action_date", "action_name", "action_body"],
    )
    attachments = ensure_columns(
        attachments,
        ["matter_id", "attachment_id", "attachment_name", "attachment_url", "attachment_type"],
    )
    indexes = ensure_columns(indexes, ["matter_id", "index_id", "index_name"])

    if not sponsors.empty:
        sponsors["matter_id"] = sponsors["matter_id"].map(normalize_matter_id)
        sponsors["person_id"] = sponsors["person_id"].map(normalize_text)
        sponsors["person_full_name"] = sponsors["person_full_name"].map(normalize_text)
        sponsors["sponsor_ward"] = sponsors["sponsor_ward"].map(normalize_text)
        sponsors["sponsor_sequence"] = pd.to_numeric(sponsors["sponsor_sequence"], errors="coerce")

    if not histories.empty:
        histories["matter_id"] = histories["matter_id"].map(normalize_matter_id)
        histories["action_seq"] = pd.to_numeric(histories["action_seq"], errors="coerce")

    if not attachments.empty:
        attachments["matter_id"] = attachments["matter_id"].map(normalize_matter_id)
        attachments["attachment_id"] = attachments["attachment_id"].map(normalize_text)
        attachments["attachment_name"] = attachments["attachment_name"].map(normalize_text)
        attachments["attachment_url"] = attachments["attachment_url"].map(normalize_text)

    persons = pd.DataFrame(columns=["person_id", "person_full_name", "person_ward"])
    if not sponsors.empty:
        persons = (
            sponsors[["person_id", "person_full_name", "sponsor_ward"]]
            .rename(columns={"sponsor_ward": "person_ward"})
            .dropna(subset=["person_id", "person_full_name"], how="all")
            .drop_duplicates()
            .reset_index(drop=True)
        )

    candidates = build_candidates(seed)

    sponsors.to_csv(args.out_sponsors, index=False)
    histories.to_csv(args.out_histories, index=False)
    attachments.to_csv(args.out_attachments, index=False)
    if args.out_indexes:
        indexes.to_csv(args.out_indexes, index=False)
    if args.out_persons:
        persons.to_csv(args.out_persons, index=False)
    candidates.to_csv(args.out_candidates, index=False)

    final_candidates = candidates[candidates["is_final_candidate"]]
    final_ids = set(final_candidates["matter_id"].dropna().tolist())
    sponsor_ids = unique_matter_ids(sponsors)
    history_ids = unique_matter_ids(histories)
    attachment_ids = unique_matter_ids(attachments)

    matters_total = int(matters["matter_id"].dropna().nunique()) if "matter_id" in matters.columns else len(matters)
    final_count = len(final_ids)

    if args.out_report_json:
        report = {
            "generated_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z"),
            "data_source": "elms",
            "api_base_url": BASE_URL,
            "input_matter_count": matters_total,
            "seed_candidate_count": int(seed["seed_is_candidate"].fillna(False).sum()),
            "final_candidate_count": final_count,
            "sponsors_rows": len(sponsors),
            "histories_rows": len(histories),
            "attachments_rows": len(attachments),
            "indexes_rows": len(indexes),
            "persons_rows": len(persons),
            "detail_resolved_by_record_number": resolved_by_record_number,
            "detail_resolved_by_guid": resolved_by_guid,
            "request_error_count": len(request_errors),
            "request_errors": request_errors,
            "coverage": {
                "sponsors_share_of_final": safe_ratio(len(final_ids & sponsor_ids), final_count),
                "histories_share_of_final": safe_ratio(len(final_ids & history_ids), final_count),
                "attachments_share_of_final": safe_ratio(len(final_ids & attachment_ids), final_count),
                "sponsors_unique_matters": len(sponsor_ids),
                "histories_unique_matters": len(history_ids),
                "attachments_unique_matters": len(attachment_ids),
            },
        }

        with open(args.out_report_json, "w", encoding="utf-8") as handle:
            json.dump(report, handle, indent=2)

    return 0


def main() -> int:
    return run_pipeline(parse_args())


if __name__ == "__main__":
    raise SystemExit(main())
