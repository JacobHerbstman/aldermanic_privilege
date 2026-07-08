import re
from datetime import date
from pathlib import Path
from urllib.parse import unquote, urlparse

import pandas as pd


DATE_PATTERN = re.compile(r"(?P<month>\d{1,2})/(?P<day>\d{1,2})/(?P<year>\d{4})")


def normalize_spaces(value: str | None) -> str:
    if value is None:
        return ""
    return " ".join(str(value).split())


def parse_meeting_date_iso(document_title: str | None) -> str | None:
    text = normalize_spaces(document_title)
    match = DATE_PATTERN.search(text)
    if match is None:
        return None

    month = int(match.group("month"))
    day = int(match.group("day"))
    year = int(match.group("year"))
    try:
        return date(year, month, day).isoformat()
    except ValueError:
        return None


def parse_meeting_type(document_title: str | None) -> tuple[str | None, str]:
    text = normalize_spaces(document_title)
    meeting_type_raw = None

    if "-" in text:
        tail = normalize_spaces(text.rsplit("-", 1)[1])
        if re.search(r"[A-Za-z]", tail):
            meeting_type_raw = tail

    scan_text = normalize_spaces(meeting_type_raw or text).lower()
    if "special" in scan_text:
        return meeting_type_raw, "special"
    if "regular" in scan_text:
        return meeting_type_raw, "regular"
    return meeting_type_raw, "other"


def sanitize_token(value: str | None) -> str:
    text = normalize_spaces(value).lower()
    text = re.sub(r"[^a-z0-9]+", "_", text).strip("_")
    return text or "journal"


def fallback_basename(pdf_url: str | None, document_title: str | None) -> str:
    if pdf_url:
        path_name = unquote(Path(urlparse(pdf_url).path).name)
        if path_name:
            stem = Path(path_name).stem
            if stem:
                return sanitize_token(stem)
    return sanitize_token(document_title)


def build_base_filename(
    meeting_date_iso: str | None,
    meeting_type_norm: str,
    fallback_name: str,
) -> str:
    if meeting_date_iso:
        date_part = meeting_date_iso.replace("-", "_")
        type_part = meeting_type_norm if meeting_type_norm in {"regular", "special"} else "other"
        return f"{date_part}_{type_part}.pdf"

    stem = sanitize_token(fallback_name)
    return f"{stem}.pdf"


def assign_collision_suffixes(filenames: list[str]) -> list[str]:
    seen: dict[str, int] = {}
    output: list[str] = []

    for name in filenames:
        path = Path(name)
        stem = sanitize_token(path.stem)
        suffix = path.suffix.lower() or ".pdf"
        base_key = f"{stem}{suffix}"

        count = seen.get(base_key, 0) + 1
        seen[base_key] = count

        if count == 1:
            output.append(f"{stem}{suffix}")
        else:
            output.append(f"{stem}_v{count}{suffix}")

    return output


def dedupe_manifest(manifest: pd.DataFrame) -> pd.DataFrame:
    if manifest.empty:
        return manifest.copy()
    deduped = manifest.drop_duplicates(subset=["year", "pdf_url"], keep="first")
    return deduped.reset_index(drop=True)


def parse_zoning_section_page(toc_text: str | None) -> int | None:
    if not toc_text:
        return None

    text = normalize_spaces(toc_text)
    patterns = [
        r"zoning(?:\s+ordinance)?(?:\s+amendments?|(?:\s+re)?classifications?)\D{0,60}(\d{1,4})",
        r"committee\s+on\s+zoning\D{0,60}(\d{1,4})",
        r"zoning\s+map\s+amendments?\D{0,60}(\d{1,4})",
        r"zoning\D{0,20}\.{2,}\s*(\d{1,4})",
    ]

    for pattern in patterns:
        match = re.search(pattern, text, flags=re.IGNORECASE)
        if not match:
            continue
        page = int(match.group(1))
        if 1 <= page <= 5000:
            return page

    for line in str(toc_text).splitlines():
        if "zoning" not in line.lower():
            continue
        match = re.search(r"(\d{1,4})\s*$", line.strip())
        if not match:
            continue
        page = int(match.group(1))
        if 1 <= page <= 5000:
            return page

    return None


def status_from_outcome(
    *,
    downloaded: bool = False,
    exists_valid: bool = False,
    http_status: int | None = None,
    timed_out: bool = False,
    non_pdf: bool = False,
    corrupt: bool = False,
    failed: bool = False,
) -> str:
    if exists_valid:
        return "exists_valid"
    if downloaded:
        return "downloaded"
    if timed_out:
        return "timeout"
    if http_status == 403:
        return "http_403"
    if http_status == 404:
        return "http_404"
    if non_pdf:
        return "non_pdf"
    if corrupt:
        return "corrupt"
    if failed:
        return "failed"
    return "failed"
