import argparse
import json
import re
import sys
from datetime import datetime, timezone
from pathlib import Path

import fitz
import pandas as pd

THIS_DIR = Path(__file__).resolve().parent
if str(THIS_DIR) not in sys.path:
    sys.path.insert(0, str(THIS_DIR))

try:
    from rezoning_parsers import extract_page_records, find_map_references, normalize_text
except Exception as exc:  # noqa: BLE001
    raise RuntimeError(f"Could not import rezoning_parsers from {THIS_DIR}: {exc}")

KEYWORD_RE = re.compile(
    r"\b(zoning|reclassif|map\s*(no\.?|number)|district|planned development|floor area ratio|\bfar\b|dwelling unit|density|height|stories?)\b",
    flags=re.IGNORECASE,
)

ADDRESS_RE = re.compile(
    r"(\b\d{1,5}(?:\s*-\s*\d{1,5})?\s+(?:[NSEW]\.?\s+)?[A-Za-z0-9\.\"' -]{1,90}?"
    r"(?:ST|STREET|AVE|AVENUE|BLVD|BOULEVARD|RD|ROAD|DR|DRIVE|CT|COURT|PL|PLACE|"
    r"PKWY|PARKWAY|TER|TERRACE|LN|LANE|WAY|CIR|CIRCLE|HWY|HIGHWAY)\b)",
    flags=re.IGNORECASE,
)

FAR_RE = re.compile(
    r"\b(?:floor\s+area\s+ratio|far)\s*(?:of|=|is|:)??\s*(\d{1,2}(?:\.\d{1,3})?)\b",
    flags=re.IGNORECASE,
)

UNITS_RE = re.compile(
    r"\b(\d{1,5})\s+(?:dwelling\s+units?|units?)\b",
    flags=re.IGNORECASE,
)

UNITS_PER_ACRE_RE = re.compile(
    r"\b(\d{1,4}(?:\.\d{1,2})?)\s*(?:units?|dwelling\s+units?)\s*(?:per|/)\s*acre\b",
    flags=re.IGNORECASE,
)

HEIGHT_RE = re.compile(
    r"\b(\d{1,3}(?:\.\d{1,2})?)\s*(?:ft|feet|foot)\s*(?:in\s+)?height\b",
    flags=re.IGNORECASE,
)

STORIES_RE = re.compile(
    r"\b(\d{1,2})\s*(?:stories|story|storeys|storey)\b",
    flags=re.IGNORECASE,
)

DENSITY_MENTION_RE = re.compile(
    r"\b(density|bulk|lot\s+area\s+per\s+unit|units?\s+per\s+acre|floor\s+area\s+ratio|\bfar\b|dwelling\s+units?)\b",
    flags=re.IGNORECASE,
)

THREE_PART_ZONE_RE = re.compile(r"\b([A-Z]{1,3})\s*-\s*(\d)\s*-\s*(\d+(?:\.\d+)?[A-Z]?)\b")
ZONING_CODE_RE = re.compile(r"\b([A-Z]{1,4}\d?)\s*[- ]?\s*(\d+(?:\.\d+)?)([A-Z]?)\b")
STREET_SUFFIX_RE = re.compile(
    r"\b(ST|STREET|AVE|AVENUE|BLVD|BOULEVARD|RD|ROAD|DR|DRIVE|CT|COURT|PL|PLACE|PKWY|PARKWAY|TER|TERRACE|LN|LANE|WAY|CIR|CIRCLE|HWY|HIGHWAY)\b",
    flags=re.IGNORECASE,
)
ADDRESS_NOISE_RE = re.compile(
    r"\b(COMMITTEE|LADIES\s+AND\s+GENTLEMEN|JOURNAL|REPORTS?\s+OF\s+COMMITTEES|CITY\s+COUNCIL|OFFICE\s+OF\s+THE\s+MAYOR|TABLE\s+OF\s+CONTENTS|CHAPTER|ARTICLE|SECTION)\b",
    flags=re.IGNORECASE,
)
ADDRESS_BOUNDARY_RE = re.compile(
    r"\b(FEET?|FT|PARALLEL|INTERSECTION|RIGHT-OF-WAY|ALLEY|NORTHEASTERLY|NORTHWESTERLY|SOUTHEASTERLY|SOUTHWESTERLY)\b",
    flags=re.IGNORECASE,
)
MAX_ADDRESS_HOUSE_NUMBER = 20000

OUTPUT_COLUMNS = [
    "journal_year",
    "journal_meeting_date",
    "journal_filename",
    "journal_rel_local_path",
    "journal_pdf_url",
    "page_number",
    "map_no",
    "match_type",
    "from_district_raw",
    "to_district_raw",
    "has_from_to",
    "from_code",
    "to_code",
    "from_far_lookup",
    "to_far_lookup",
    "far_change_lookup",
    "addresses_strict",
    "addresses_loose",
    "addresses",
    "far_values",
    "max_far_value",
    "dwelling_unit_counts",
    "units_per_acre_values",
    "story_counts",
    "height_ft_values",
    "density_mention_count",
    "snippet",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-manifest-csv", required=True)
    parser.add_argument("--in-journals-dir", required=True)
    parser.add_argument("--in-zoning-lookup-csv", required=True)
    parser.add_argument("--year-start", type=int)
    parser.add_argument("--year-end", type=int)
    parser.add_argument("--years-csv")
    parser.add_argument("--use-zoning-start-page-window", type=int, default=1)
    parser.add_argument("--zoning-start-page-buffer", type=int, default=3)
    parser.add_argument("--max-pages-after-zoning-start", type=int, default=120)
    parser.add_argument("--fallback-max-pages-no-start", type=int, default=120)
    parser.add_argument("--max-pdfs", type=int, default=0)
    parser.add_argument("--out-features-csv", required=True)
    parser.add_argument("--out-summary-json")
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


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


def parse_years_filter(args: argparse.Namespace) -> set[int] | None:
    if args.years_csv:
        years = {int(part.strip()) for part in args.years_csv.split(",") if part.strip()}
        return years if years else None
    if args.year_start is None and args.year_end is None:
        return None
    if args.year_start is None or args.year_end is None:
        raise ValueError("Provide both --year-start and --year-end, or use --years-csv.")
    if args.year_start > args.year_end:
        raise ValueError("--year-start cannot be greater than --year-end.")
    return set(range(args.year_start, args.year_end + 1))


def parse_int_or_none(value) -> int | None:
    if pd.isna(value):
        return None
    try:
        return int(float(str(value).strip()))
    except Exception:  # noqa: BLE001
        return None


def safe_float(value) -> float | None:
    if pd.isna(value):
        return None
    try:
        return float(str(value).strip())
    except Exception:  # noqa: BLE001
        return None


def clean_text(value) -> str | None:
    if value is None or pd.isna(value):
        return None
    out = normalize_text(str(value))
    out = re.sub(r"\s+", " ", out).strip(" ,;.-")
    return out or None


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


def normalize_numeric_token(value: str | None) -> str:
    if not value:
        return ""
    out = str(value).upper().strip()
    return out.replace("I", "1").replace("L", "1").replace("O", "0").replace("S", "5")


def parse_zoning_code(value) -> str | None:
    text = clean_zone_text(value)
    if not text:
        return None

    text = text.replace("B1.4", "B1-4").replace("M1.-4", "M1-4")
    text = text.replace("B24", "B2-4").replace("C24", "C2-4")
    text = re.sub(r"\bRMS\.?5\b", "RM5.5", text)
    text = re.sub(r"\bRMS\b", "RM5", text)
    text = re.sub(r"\bRTS\.?5\b", "RT3.5", text)
    text = re.sub(r"\bDXS\b", "DX5", text)
    text = re.sub(r"\b(DR|DX|DC|DS)1-(\d)\b", r"\1-1\2", text)

    if "PLANNED DEVELOPMENT" in text or re.search(r"\bPD\b", text):
        return "PD"
    if re.search(r"\bPOS\b", text) or "PARKS" in text or "OPEN SPACE" in text:
        return "POS"
    if text == "T" or text.startswith("T TRANSPORTATION"):
        return "T"
    if re.search(r"\bC\s*[- ]?4\b", text):
        return "C4"

    old_r = re.search(r"\bR[- ]?([1-8ILS])\b(?!\s*-\s*\d)", text)
    if old_r:
        digit = normalize_numeric_token(old_r.group(1))
        if re.fullmatch(r"[1-8]", digit):
            return f"R{digit}"

    modern_res = re.search(r"\b(RS|RT|RM)\s*[- ]?\s*([0-9ILSO](?:\.[0-9ILSO])?)([A-Z]?)\b", text)
    if modern_res:
        prefix = modern_res.group(1)
        number = normalize_numeric_token(modern_res.group(2))
        suffix = modern_res.group(3)
        code = f"{prefix}-{number}{suffix}"
    else:
        modern_downtown = re.search(r"\b(DR|DX|DC|DS)\s*[- ]?\s*([0-9ILSO](?:\.[0-9ILSO])?)\b", text)
        if modern_downtown:
            prefix = modern_downtown.group(1)
            number = normalize_numeric_token(modern_downtown.group(2))
            code = f"{prefix}-{number}"
        else:
            code = None

    if code is None:
        three_part = THREE_PART_ZONE_RE.search(text)
        if three_part:
            prefix = three_part.group(1)
            mid = normalize_numeric_token(three_part.group(2))
            num = normalize_numeric_token(three_part.group(3))
            code = f"{prefix}{mid}-{num}"
        else:
            explicit_bcm = re.search(r"\b([BCM8])\s*([1-7ILSO])\s*[-\. ]\s*([0-9ILSO](?:\.[0-9ILSO])?)\b", text)
            if explicit_bcm:
                fam = "B" if explicit_bcm.group(1) == "8" else explicit_bcm.group(1)
                mid = normalize_numeric_token(explicit_bcm.group(2))
                num = normalize_numeric_token(explicit_bcm.group(3))
                code = f"{fam}{mid}-{num}"

    if code is None:
        compact_bcm = re.search(r"\b([BCM8])([1-7ILSO])([0-9ILSO])\b", text)
        if compact_bcm:
            fam = "B" if compact_bcm.group(1) == "8" else compact_bcm.group(1)
            mid = normalize_numeric_token(compact_bcm.group(2))
            num = normalize_numeric_token(compact_bcm.group(3))
            code = f"{fam}{mid}-{num}"

    if code is None:
        match = ZONING_CODE_RE.search(text)
        if not match:
            return None
        prefix = match.group(1).replace("8", "B")
        number = normalize_numeric_token(match.group(2))
        suffix = normalize_numeric_token(match.group(3))
        code = f"{prefix}-{number}{suffix}"

    code = code.replace("--", "-")
    code = re.sub(r"\s+", "", code)
    code = code.replace("-L", "-1")
    code = code.replace("-I", "-1")
    code = code.replace("-S", "-5")
    code = code.replace("B1.4", "B1-4")
    code = code.replace("M1.-4", "M1-4")
    code = code.replace("B24", "B2-4")
    code = code.replace("C24", "C2-4")
    code = re.sub(r"^([BCM])([1-7])\.(\d+(?:\.\d+)?)$", r"\1\2-\3", code)
    code = re.sub(r"^AB([1-7]-\d+(?:\.\d+)?)$", r"B\1", code)
    code = re.sub(r"^ABL-(\d+(?:\.\d+)?)$", r"BL-\1", code)
    code = re.sub(r"^(DR|DX|DC|DS)1-(\d)$", r"\1-1\2", code)
    code = re.sub(r"^R-(\d)$", r"R\1", code)

    if code == "RS3":
        code = "RS-3"
    if code == "RM-4":
        code = "RM-4.5"
    if code == "M-1":
        code = "M1-1"
    return code


def parse_lookup_date(value) -> pd.Timestamp | None:
    ts = pd.to_datetime(value, errors="coerce")
    if pd.isna(ts):
        return None
    return ts


def load_far_lookup(path: str) -> dict[str, list[dict]]:
    df = pd.read_csv(path, dtype=str, low_memory=False)
    cols = df.columns.tolist()

    code_col = None
    for name in ["zone_code", "district", "district_type", "code", "zoning_code"]:
        if name in cols:
            code_col = name
            break
    if code_col is None:
        code_col = cols[0]

    far_col = None
    for name in ["max_far", "far", "maximum_far", "floor_area_ratio"]:
        if name in cols:
            far_col = name
            break
    if far_col is None:
        far_col = next((c for c in cols if "far" in c.lower()), None)
    if far_col is None:
        raise ValueError("Could not identify FAR column in zoning lookup table.")

    start_col = next((c for c in cols if c.lower() in {"effective_start_date", "start_date", "valid_from"}), None)
    end_col = next((c for c in cols if c.lower() in {"effective_end_date", "end_date", "valid_to"}), None)

    lookup: dict[str, list[dict]] = {}
    for _, row in df.iterrows():
        code = parse_zoning_code(row.get(code_col))
        far = safe_float(row.get(far_col))
        if code and far is not None:
            lookup.setdefault(code, []).append(
                {
                    "far": far,
                    "effective_start": parse_lookup_date(row.get(start_col)) if start_col else None,
                    "effective_end": parse_lookup_date(row.get(end_col)) if end_col else None,
                }
            )

    has_effective_dates = bool(start_col or end_col)

    def append_manual(code: str, far: float) -> None:
        if has_effective_dates:
            record = {
                "far": far,
                "effective_start": pd.Timestamp("2004-11-01"),
                "effective_end": None,
            }
        else:
            record = {
                "far": far,
                "effective_start": None,
                "effective_end": None,
            }
        lookup.setdefault(code, []).append(record)

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
    return lookup


def far_for_code(code: str | None, lookup: dict[str, list[dict]], reference_date=None) -> float | None:
    if not code or code in {"PD", "POS"}:
        return None

    records = lookup.get(code)
    if not records:
        return None

    ts = pd.to_datetime(reference_date, errors="coerce")
    if not pd.isna(ts):
        applicable = []
        for rec in records:
            start = rec.get("effective_start")
            end = rec.get("effective_end")
            if start is not None and ts < start:
                continue
            if end is not None and ts > end:
                continue
            applicable.append(rec)
        if applicable:
            applicable.sort(
                key=lambda rec: rec.get("effective_start") if rec.get("effective_start") is not None else pd.Timestamp.min,
                reverse=True,
            )
            return applicable[0].get("far")
        if ts < pd.Timestamp("2004-11-01"):
            rs_match = re.fullmatch(r"RS-(\d)", code)
            if rs_match:
                return far_for_code(f"R{rs_match.group(1)}", lookup, reference_date=reference_date)

    return records[0].get("far")


def join_unique(values: list, *, cast=str) -> str | None:
    seen = set()
    out: list[str] = []
    for value in values:
        if value is None:
            continue
        try:
            item = cast(value)
        except Exception:  # noqa: BLE001
            continue
        key = str(item)
        if key in seen:
            continue
        seen.add(key)
        out.append(key)
    return "; ".join(out) if out else None


def _normalize_address_candidate_base(value: str | None) -> str | None:
    candidate = clean_text(value)
    if not candidate:
        return None

    candidate = re.sub(r"\b([NSEW])\.\b", r"\1", candidate, flags=re.IGNORECASE)
    candidate = re.sub(r"^\s*MAP\s*(?:NO\.?|NUMBER)\s*[0-9A-Z/-]+\s*", "", candidate, flags=re.IGNORECASE)
    candidate = re.sub(
        r"^\s*[0-9A-Z/-]+\s+FOR\s+THE\s+PROPERTY\s+LOCATED\s+AT\s+",
        "",
        candidate,
        flags=re.IGNORECASE,
    )
    candidate = re.sub(r"^\s*(?:FOR\s+THE\s+PROPERTY\s+LOCATED\s+AT|PROPERTY\s+LOCATED\s+AT)\s+", "", candidate, flags=re.IGNORECASE)
    candidate = re.sub(r"\s+", " ", candidate).strip(" ,.;")
    if not candidate:
        return None

    if ADDRESS_NOISE_RE.search(candidate):
        return None
    if STREET_SUFFIX_RE.search(candidate) is None:
        return None

    house_match = re.match(r"^(\d{1,5})(?:\s*-\s*(\d{1,5}))?\b", candidate)
    if house_match is None:
        return None

    try:
        low_num = int(house_match.group(1))
    except Exception:  # noqa: BLE001
        return None
    if low_num <= 0 or low_num > MAX_ADDRESS_HOUSE_NUMBER:
        return None

    if house_match.group(2):
        try:
            high_num = int(house_match.group(2))
        except Exception:  # noqa: BLE001
            return None
        if high_num <= 0 or high_num > MAX_ADDRESS_HOUSE_NUMBER:
            return None

    token_count = len(candidate.split())
    if token_count < 3 or token_count > 12:
        return None

    return candidate


def normalize_address_candidate_strict(value: str | None) -> str | None:
    candidate = _normalize_address_candidate_base(value)
    if not candidate:
        return None
    if ADDRESS_BOUNDARY_RE.search(candidate):
        return None
    return candidate


def normalize_address_candidate_loose(value: str | None) -> str | None:
    candidate = clean_text(value)
    if not candidate:
        return None
    candidate = re.sub(
        r"^\s*\d{1,5}\s*(?:FEET?|FT)\s+(?:NORTH|SOUTH|EAST|WEST)\s+OF\s+",
        "",
        candidate,
        flags=re.IGNORECASE,
    )
    return _normalize_address_candidate_base(candidate)


def normalize_address_candidate(value: str | None) -> str | None:
    # Backward-compatible alias for strict extraction.
    return normalize_address_candidate_strict(value)


def extract_addresses_with_normalizer(text: str, normalizer, max_n: int = 8) -> list[str]:
    found: list[str] = []
    seen = set()
    for match in ADDRESS_RE.finditer(text or ""):
        value = normalizer(match.group(1))
        if not value:
            continue
        key = value.upper()
        if key in seen:
            continue
        seen.add(key)
        found.append(value)
        if len(found) >= max_n:
            break
    return found


def extract_addresses_strict(text: str, max_n: int = 8) -> list[str]:
    return extract_addresses_with_normalizer(text=text, normalizer=normalize_address_candidate_strict, max_n=max_n)


def extract_addresses_loose(text: str, max_n: int = 8) -> list[str]:
    return extract_addresses_with_normalizer(text=text, normalizer=normalize_address_candidate_loose, max_n=max_n)


def extract_addresses(text: str, max_n: int = 8) -> list[str]:
    # Backward-compatible function used by existing tests/integration.
    return extract_addresses_strict(text=text, max_n=max_n)


def _extract_numeric(pattern: re.Pattern, text: str, *, as_int: bool = False) -> list[float | int]:
    values: list[float | int] = []
    for match in pattern.findall(text or ""):
        try:
            raw = str(match).replace(",", "").strip()
            if as_int:
                values.append(int(float(raw)))
            else:
                values.append(float(raw))
        except Exception:  # noqa: BLE001
            continue
    return values


def extract_metric_lists(text: str) -> dict:
    far_values = [v for v in _extract_numeric(FAR_RE, text, as_int=False) if 0 < v <= 50]
    dwelling_units = [v for v in _extract_numeric(UNITS_RE, text, as_int=True) if 0 < v <= 200000]
    units_per_acre = [v for v in _extract_numeric(UNITS_PER_ACRE_RE, text, as_int=False) if 0 < v <= 10000]
    story_counts = [v for v in _extract_numeric(STORIES_RE, text, as_int=True) if 0 < v <= 200]
    height_ft = [v for v in _extract_numeric(HEIGHT_RE, text, as_int=False) if 0 < v <= 2000]
    density_mentions = len(DENSITY_MENTION_RE.findall(text or ""))

    return {
        "far_values": far_values,
        "max_far_value": max(far_values) if far_values else None,
        "dwelling_unit_counts": dwelling_units,
        "units_per_acre_values": units_per_acre,
        "story_counts": story_counts,
        "height_ft_values": height_ft,
        "density_mention_count": density_mentions,
    }


def pick_page_window(
    *,
    doc_page_count: int,
    zoning_section_page: int | None,
    use_window: bool,
    buffer_pages: int,
    max_pages_after_start: int,
    fallback_max_pages_no_start: int,
) -> tuple[int, int, str]:
    if doc_page_count <= 0:
        return 0, 0, "empty"

    if use_window and zoning_section_page is not None and zoning_section_page > 0:
        start_idx = max(0, zoning_section_page - 1 - max(buffer_pages, 0))
        end_idx = min(doc_page_count, start_idx + max(max_pages_after_start, 1))
        return start_idx, end_idx, "zoning_start_window"

    if fallback_max_pages_no_start > 0:
        return 0, min(doc_page_count, fallback_max_pages_no_start), "fallback_front_window"

    return 0, doc_page_count, "full_pdf"


def row_from_record(
    base: dict,
    record: dict,
    metrics: dict,
    addresses_strict: list[str],
    addresses_loose: list[str],
    map_refs: list[str],
    far_lookup: dict[str, list[dict]],
) -> dict:
    map_no = clean_text(record.get("map_no"))
    if not map_no and map_refs:
        map_no = clean_text(map_refs[0])

    from_raw = clean_text(record.get("from_district_raw"))
    to_raw = clean_text(record.get("to_district_raw"))
    from_code = parse_zoning_code(from_raw)
    to_code = parse_zoning_code(to_raw)
    reference_date = base.get("journal_meeting_date")
    from_far = far_for_code(from_code, far_lookup, reference_date=reference_date)
    to_far = far_for_code(to_code, far_lookup, reference_date=reference_date)
    far_change = (to_far - from_far) if (to_far is not None and from_far is not None) else None

    out = {
        **base,
        "map_no": map_no,
        "match_type": clean_text(record.get("match_type")),
        "from_district_raw": from_raw,
        "to_district_raw": to_raw,
        "has_from_to": bool(record.get("has_from_to")),
        "from_code": from_code,
        "to_code": to_code,
        "from_far_lookup": from_far,
        "to_far_lookup": to_far,
        "far_change_lookup": far_change,
        "addresses_strict": join_unique(addresses_strict),
        "addresses_loose": join_unique(addresses_loose),
        "addresses": join_unique(addresses_strict),
        "far_values": join_unique(metrics["far_values"]),
        "max_far_value": metrics["max_far_value"],
        "dwelling_unit_counts": join_unique(metrics["dwelling_unit_counts"]),
        "units_per_acre_values": join_unique(metrics["units_per_acre_values"]),
        "story_counts": join_unique(metrics["story_counts"]),
        "height_ft_values": join_unique(metrics["height_ft_values"]),
        "density_mention_count": metrics["density_mention_count"],
        "snippet": clean_text(record.get("snippet")),
    }
    return out


def row_from_keyword_page(
    base: dict,
    metrics: dict,
    addresses_strict: list[str],
    addresses_loose: list[str],
    map_refs: list[str],
    page_text: str,
) -> dict:
    map_no = clean_text(map_refs[0]) if map_refs else None
    snippet = clean_text((page_text or "")[:450])
    return {
        **base,
        "map_no": map_no,
        "match_type": "keyword_page",
        "from_district_raw": None,
        "to_district_raw": None,
        "has_from_to": False,
        "from_code": None,
        "to_code": None,
        "from_far_lookup": None,
        "to_far_lookup": None,
        "far_change_lookup": None,
        "addresses_strict": join_unique(addresses_strict),
        "addresses_loose": join_unique(addresses_loose),
        "addresses": join_unique(addresses_strict),
        "far_values": join_unique(metrics["far_values"]),
        "max_far_value": metrics["max_far_value"],
        "dwelling_unit_counts": join_unique(metrics["dwelling_unit_counts"]),
        "units_per_acre_values": join_unique(metrics["units_per_acre_values"]),
        "story_counts": join_unique(metrics["story_counts"]),
        "height_ft_values": join_unique(metrics["height_ft_values"]),
        "density_mention_count": metrics["density_mention_count"],
        "snippet": snippet,
    }


def build_summary(
    out_df: pd.DataFrame,
    *,
    manifest_rows_considered: int,
    pdfs_scanned: int,
    pages_scanned: int,
    pages_keyword_hits: int,
    page_window_mode_counts: dict,
) -> dict:
    rows_with_from_to = int(out_df["has_from_to"].fillna(False).map(parse_bool).sum()) if not out_df.empty else 0
    rows_with_far = int(out_df["far_values"].fillna("").ne("").sum()) if not out_df.empty else 0
    rows_with_far_lookup_both = int((out_df["from_far_lookup"].notna() & out_df["to_far_lookup"].notna()).sum()) if not out_df.empty else 0
    rows_with_addresses_strict = int(out_df["addresses_strict"].fillna("").ne("").sum()) if not out_df.empty else 0
    rows_with_addresses_loose = int(out_df["addresses_loose"].fillna("").ne("").sum()) if not out_df.empty else 0
    rows_where_loose_added_coverage = (
        int(
            (
                out_df["addresses_loose"].fillna("").ne("")
                & out_df["addresses_strict"].fillna("").eq("")
            ).sum()
        )
        if not out_df.empty
        else 0
    )
    rows_with_density = (
        int(
            (
                out_df["dwelling_unit_counts"].fillna("").ne("")
                | out_df["units_per_acre_values"].fillna("").ne("")
                | out_df["story_counts"].fillna("").ne("")
                | out_df["height_ft_values"].fillna("").ne("")
                | pd.to_numeric(out_df["density_mention_count"], errors="coerce").fillna(0).gt(0)
            ).sum()
        )
        if not out_df.empty
        else 0
    )

    by_year = (
        out_df.groupby("journal_year").size().astype(int).to_dict()
        if not out_df.empty
        else {}
    )

    return {
        "generated_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z"),
        "manifest_rows_considered": int(manifest_rows_considered),
        "pdfs_scanned": int(pdfs_scanned),
        "pages_scanned": int(pages_scanned),
        "pages_keyword_hits": int(pages_keyword_hits),
        "rows_output": int(len(out_df)),
        "rows_with_from_to": rows_with_from_to,
        "rows_with_far_mentions": rows_with_far,
        "rows_with_far_lookup_both": rows_with_far_lookup_both,
        "rows_with_addresses": rows_with_addresses_strict,
        "rows_with_addresses_strict": rows_with_addresses_strict,
        "rows_with_addresses_loose": rows_with_addresses_loose,
        "rows_where_loose_added_coverage": rows_where_loose_added_coverage,
        "rows_with_density_signals": rows_with_density,
        "rows_by_year": {str(k): int(v) for k, v in by_year.items()},
        "page_window_mode_counts": {str(k): int(v) for k, v in page_window_mode_counts.items()},
    }


def main() -> int:
    args = parse_args()
    ensure_parent(args.out_features_csv)
    if args.out_summary_json:
        ensure_parent(args.out_summary_json)

    year_filter = parse_years_filter(args)
    manifest = pd.read_csv(args.in_manifest_csv, dtype=str, low_memory=False)

    keep = manifest.copy()
    if "journal_year" not in keep.columns:
        keep["journal_year"] = pd.to_numeric(keep.get("year"), errors="coerce").astype("Int64")
    else:
        keep["journal_year"] = pd.to_numeric(keep["journal_year"], errors="coerce").astype("Int64")

    if year_filter is not None:
        keep = keep[keep["journal_year"].isin(list(year_filter))]

    keep = keep[
        keep.get("downloaded", pd.Series(dtype=str)).map(parse_bool)
        & keep.get("is_valid_pdf", pd.Series(dtype=str)).map(parse_bool)
    ].copy()

    keep = keep.sort_values(["journal_year", "meeting_date", "filename"], kind="stable")
    if args.max_pdfs and args.max_pdfs > 0:
        keep = keep.head(args.max_pdfs)
    keep = keep.reset_index(drop=True)

    journals_dir = Path(args.in_journals_dir)
    far_lookup = load_far_lookup(args.in_zoning_lookup_csv)

    rows: list[dict] = []
    pages_scanned = 0
    pages_keyword_hits = 0
    pdfs_scanned = 0
    page_window_mode_counts: dict[str, int] = {}

    for idx, row in keep.iterrows():
        rel_path = str(row["journal_rel_local_path"] if "journal_rel_local_path" in row else row["rel_local_path"])
        pdf_path = journals_dir / rel_path
        if not pdf_path.exists():
            continue

        try:
            doc = fitz.open(str(pdf_path))
        except Exception:  # noqa: BLE001
            continue

        print(f"[{idx + 1}/{len(keep)}] parsing {rel_path}")
        pdfs_scanned += 1

        start_idx, end_idx, mode = pick_page_window(
            doc_page_count=doc.page_count,
            zoning_section_page=parse_int_or_none(row.get("zoning_section_page")),
            use_window=bool(args.use_zoning_start_page_window),
            buffer_pages=int(args.zoning_start_page_buffer),
            max_pages_after_start=int(args.max_pages_after_zoning_start),
            fallback_max_pages_no_start=int(args.fallback_max_pages_no_start),
        )
        page_window_mode_counts[mode] = page_window_mode_counts.get(mode, 0) + 1

        for page_idx in range(start_idx, end_idx):
            page = doc.load_page(page_idx)
            page_text = page.get_text("text") or ""
            pages_scanned += 1

            if not page_text.strip() or KEYWORD_RE.search(page_text) is None:
                continue

            pages_keyword_hits += 1
            page_number = page_idx + 1

            records = extract_page_records(page_text=page_text, page_number=page_number)
            map_refs = find_map_references(page_text)
            page_addresses_strict = extract_addresses_strict(page_text)
            page_addresses_loose = extract_addresses_loose(page_text)
            page_metrics = extract_metric_lists(page_text)

            base = {
                "journal_year": str(int(row["journal_year"])) if pd.notna(row.get("journal_year")) else None,
                "journal_meeting_date": clean_text(row.get("meeting_date")),
                "journal_filename": clean_text(row.get("filename")),
                "journal_rel_local_path": rel_path,
                "journal_pdf_url": clean_text(row.get("pdf_url")),
                "page_number": page_number,
            }

            if records:
                for record in records:
                    snippet_text = clean_text(record.get("snippet")) or ""
                    snippet_addresses_strict = extract_addresses_strict(snippet_text)
                    snippet_addresses_loose = extract_addresses_loose(snippet_text)
                    addresses_strict = (
                        snippet_addresses_strict if snippet_addresses_strict else page_addresses_strict
                    )
                    addresses_loose = (
                        snippet_addresses_loose if snippet_addresses_loose else page_addresses_loose
                    )

                    snippet_metrics = extract_metric_lists(snippet_text) if snippet_text else {}
                    has_snippet_metrics = bool(
                        snippet_metrics
                        and (
                            snippet_metrics.get("far_values")
                            or snippet_metrics.get("dwelling_unit_counts")
                            or snippet_metrics.get("units_per_acre_values")
                            or snippet_metrics.get("story_counts")
                            or snippet_metrics.get("height_ft_values")
                            or (snippet_metrics.get("density_mention_count") or 0) > 0
                        )
                    )
                    metrics = snippet_metrics if has_snippet_metrics else page_metrics

                    rows.append(
                        row_from_record(
                            base,
                            record,
                            metrics,
                            addresses_strict,
                            addresses_loose,
                            map_refs,
                            far_lookup,
                        )
                    )
            else:
                has_any_signal = (
                    bool(page_addresses_strict) or bool(page_addresses_loose)
                    or bool(map_refs)
                    or bool(page_metrics["far_values"])
                    or bool(page_metrics["dwelling_unit_counts"])
                    or bool(page_metrics["story_counts"])
                    or bool(page_metrics["height_ft_values"])
                    or bool(page_metrics["units_per_acre_values"])
                    or page_metrics["density_mention_count"] > 0
                )
                if has_any_signal:
                    rows.append(
                        row_from_keyword_page(
                            base,
                            page_metrics,
                            page_addresses_strict,
                            page_addresses_loose,
                            map_refs,
                            page_text,
                        )
                    )

        doc.close()

    out = pd.DataFrame(rows)
    if out.empty:
        out = pd.DataFrame(columns=OUTPUT_COLUMNS)
    else:
        for col in OUTPUT_COLUMNS:
            if col not in out.columns:
                out[col] = None
        out = out[OUTPUT_COLUMNS].copy()

    out.to_csv(args.out_features_csv, index=False)

    if args.out_summary_json:
        summary = build_summary(
            out,
            manifest_rows_considered=len(keep),
            pdfs_scanned=pdfs_scanned,
            pages_scanned=pages_scanned,
            pages_keyword_hits=pages_keyword_hits,
            page_window_mode_counts=page_window_mode_counts,
        )

        with open(args.out_summary_json, "w", encoding="utf-8") as handle:
            json.dump(summary, handle, indent=2)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
