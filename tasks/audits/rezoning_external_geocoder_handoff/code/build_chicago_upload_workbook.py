import argparse
import re
from pathlib import Path

import pandas as pd

AT_RE = re.compile(r"\bat\b\s+(.+?)(?:\s*-\s*App\b|\s*$)", re.IGNORECASE)
BND_RE = re.compile(r"\bbounded by\b\s+(.+?)(?:\s*-\s*App\b|\s*$)", re.IGNORECASE)
STREET_SUFFIXES = (
    "ST",
    "STREET",
    "AVE",
    "AVENUE",
    "BLVD",
    "BOULEVARD",
    "RD",
    "ROAD",
    "DR",
    "DRIVE",
    "CT",
    "COURT",
    "PL",
    "PLACE",
    "PKWY",
    "PARKWAY",
    "TER",
    "TERR",
    "TERRACE",
    "LN",
    "LANE",
    "WAY",
    "CIR",
    "CIRCLE",
    "HWY",
    "HIGHWAY",
    "MARKET",
    "PLAZA",
    "PLZ",
)
STREET_SUFFIX_SET = {suffix.upper() for suffix in STREET_SUFFIXES}
SUFFIX_PATTERN = r"(?:%s)" % "|".join(STREET_SUFFIXES)
DIR_PATTERN = r"(?:N|S|E|W|NORTH|SOUTH|EAST|WEST)"
STREET_TOKEN = r"[A-Z0-9][A-Z0-9'\.-]*"
STREET_SEGMENT = rf"(?:{DIR_PATTERN}\s+)?{STREET_TOKEN}(?:\s+{STREET_TOKEN})*\s+{SUFFIX_PATTERN}(?:\s+{DIR_PATTERN})?"
STREET_SEGMENT_SOFT = rf"(?:{DIR_PATTERN}\s+)?{STREET_TOKEN}(?:\s+{STREET_TOKEN}){{0,4}}(?:\s+{SUFFIX_PATTERN})?(?:\s+{DIR_PATTERN})?"
ADDR_RE = re.compile(
    rf"(\b\d{{1,5}}(?:-\d{{1,5}})?\s+(?:{DIR_PATTERN}\s+)?[A-Za-z0-9\.\-'\" ]{{1,80}}?"
    rf"\b{SUFFIX_PATTERN}\b)",
    re.IGNORECASE,
)
STREET_ADDRESS_RE = re.compile(rf"^\d{{1,6}}(?:-\d{{1,6}})?\s+{STREET_SEGMENT}$", re.IGNORECASE)
STREET_ADDRESS_SOFT_RE = re.compile(rf"^\d{{1,6}}(?:-\d{{1,6}})?\s+{STREET_SEGMENT_SOFT}$", re.IGNORECASE)
INTERSECTION_RE = re.compile(rf"^{STREET_SEGMENT}\s+(?:AND|&)\s+{STREET_SEGMENT}$", re.IGNORECASE)
INTERSECTION_SOFT_RE = re.compile(rf"^{STREET_SEGMENT_SOFT}\s+(?:AND|&)\s+{STREET_SEGMENT_SOFT}$", re.IGNORECASE)
RELATIVE_RE = re.compile(
    r"\b("
    r"feet|foot|line|right[- ]of[- ]way|intersection of|northwesterly|northeasterly|"
    r"southwesterly|southeasterly|perpendicular|thereof|extended|along|from said|to said|"
    r"where no street|parcel\b|district\b|planned development|map no\.?|application no\.?"
    r")\b",
    re.IGNORECASE,
)
CITY_STATE_RE = re.compile(r",?\s*CHICAGO\s*,?\s*IL(?:LINOIS)?\s*$", re.IGNORECASE)
GLUED_DIR_RE = re.compile(r"\b(OF|FROM|TO)(NORTH|SOUTH|EAST|WEST)\b", re.IGNORECASE)
COMPACT_DIGITS_RE = re.compile(r"\b\d(?:\s+\d){1,5}\b")
APP_NO_TRAIL_RE = re.compile(r"\bAPP(?:LICATION)?\.?\s*NO\.?\s*[A-Z0-9-]+(?:\s+[A-Z0-9-]+)*\b.*$", re.IGNORECASE)
OCR_WORD_FIXES = {
    "PAUUNA": "PAULINA",
    "INDIANAPOUS": "INDIANAPOLIS",
    "WESTEM": "WESTERN",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-unmatched-csv", required=True)
    parser.add_argument("--in-stage1-csv", required=True)
    parser.add_argument("--out-upload-xlsx", required=True)
    parser.add_argument("--out-mapping-csv", required=True)
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def normalize_chunk(txt: str) -> str:
    out = txt.strip(" -;,.\t\n")
    out = re.sub(r"\bsnd\b", "and", out, flags=re.IGNORECASE)
    out = APP_NO_TRAIL_RE.sub("", out)
    out = out.replace("\r", " ").replace("\n", " ")
    out = GLUED_DIR_RE.sub(r"\1 \2", out)
    out = re.sub(r"\s+", " ", out)
    return out.strip(" ,;.-")


def first_intersectionish(chunk: str) -> str:
    cleaned = normalize_chunk(chunk)
    cleaned = re.sub(r"\bfrom\b", " and ", cleaned, flags=re.IGNORECASE)
    cleaned = re.sub(r"\bto\b", " and ", cleaned, flags=re.IGNORECASE)
    cleaned = re.sub(r"\s+at\s+", " and ", cleaned, flags=re.IGNORECASE)

    comma_parts = [part.strip() for part in re.split(r"\s*,\s*", cleaned) if part.strip()]
    if len(comma_parts) >= 2:
        return f"{comma_parts[0]} and {comma_parts[1]}"

    and_parts = [part.strip() for part in re.split(r"\s+and\s+", cleaned, flags=re.IGNORECASE) if part.strip()]
    if len(and_parts) >= 2:
        return f"{and_parts[0]} and {and_parts[1]}"

    return cleaned


def is_bad_addr(value) -> bool:
    if pd.isna(value):
        return True
    text = str(value).strip()
    if not text:
        return True
    lowered = text.lower()
    return lowered in {
        "nan, chicago, il",
        "na, chicago, il",
        "none, chicago, il",
        ", chicago, il",
        "chicago, il",
        "nan",
        "na",
        "none",
    }


def apply_ocr_word_fixes(text: str) -> str:
    out = text
    for bad, good in OCR_WORD_FIXES.items():
        out = re.sub(rf"\b{re.escape(bad)}\b", good, out, flags=re.IGNORECASE)
    return out


def normalize_upload_address(value: str) -> str:
    out = normalize_chunk(value)
    out = apply_ocr_word_fixes(out)
    out = APP_NO_TRAIL_RE.sub("", out)
    out = re.sub(r"\b(\d{1,6})([NSEW])(?=[A-Z])", r"\1 \2 ", out)
    out = re.sub(r"\b(\d{1,6})(NORTH|SOUTH|EAST|WEST)(?=[A-Z])", r"\1 \2 ", out, flags=re.IGNORECASE)
    out = out.replace("/", " / ")
    out = out.replace(".", " ")
    out = re.sub(r"^\s*(19|20)\d{2}\s+(?=\d{1,6}\b)", "", out)
    out = re.sub(r"^\s*(\d{1,6})\s+TO\s+(\d{1,6})\b", r"\1-\2", out, flags=re.IGNORECASE)
    out = re.sub(r"\b(NORTH|SOUTH|EAST|WEST)\s+\1\b", r"\1", out, flags=re.IGNORECASE)
    out = COMPACT_DIGITS_RE.sub(lambda m: m.group(0).replace(" ", ""), out)
    out = re.sub(r"\s+", " ", out).strip(" ,")
    out = CITY_STATE_RE.sub("", out).strip(" ,")
    if not out:
        return "Chicago, IL"
    return f"{out}, Chicago, IL"


def extract_address_chunks(chunk: str) -> list[str]:
    cleaned = normalize_chunk(chunk)
    cleaned = apply_ocr_word_fixes(cleaned)
    cleaned = re.sub(r"\(.*?\)", " ", cleaned)
    cleaned = re.sub(r"\ba\.?k\.?a\.?\b", " ", cleaned, flags=re.IGNORECASE)
    cleaned = re.sub(r"\bcommonly known as\b", " ", cleaned, flags=re.IGNORECASE)
    cleaned = re.sub(r"\s+", " ", cleaned).strip(" ,;")
    if not cleaned:
        return []

    pieces = []
    for part in re.split(r"\s*(?:,|;|/|\band\b)\s*", cleaned, flags=re.IGNORECASE):
        part = normalize_chunk(part)
        if not part:
            continue
        if not re.search(r"\b\d{1,6}\b", part):
            continue
        # Keep only the first likely address segment in noisy fragments.
        m = re.search(r"\b\d{1,6}(?:-\d{1,6})?\s+.*", part)
        piece = m.group(0) if m else part
        pieces.append(piece)

    out: list[str] = []
    seen = set()
    for piece in pieces:
        norm = normalize_chunk(piece)
        if not norm:
            continue
        key = norm.upper()
        if key in seen:
            continue
        seen.add(key)
        out.append(norm)
    return out


def street_name_quality_ok(street_core: str, require_suffix: bool = True) -> bool:
    tokens = [t.strip(" ,.;") for t in street_core.upper().split() if t.strip(" ,.;")]
    if not tokens:
        return False
    if tokens[-1] in {"N", "S", "E", "W", "NORTH", "SOUTH", "EAST", "WEST"}:
        tokens = tokens[:-1]
    if not tokens:
        return False
    if require_suffix:
        if tokens[-1] not in STREET_SUFFIX_SET:
            return False
        tokens = tokens[:-1]
    if tokens and tokens[0] in {"N", "S", "E", "W", "NORTH", "SOUTH", "EAST", "WEST"}:
        tokens = tokens[1:]
    if not tokens:
        return False

    banned = {"THE", "ONE", "WAY", "LINE", "PARALLEL", "PERPENDICULAR", "THEREOF", "DISTRICT", "RIGHT-OF-WAY", "TO"}
    if any(token.isdigit() for token in tokens):
        return False
    alpha_tokens = [re.sub(r"[^A-Z]", "", token) for token in tokens]
    alpha_tokens = [token for token in alpha_tokens if token]
    if not alpha_tokens:
        return False
    # Soft matching (no required suffix) must still contain a real street name token.
    if not require_suffix and all(token in STREET_SUFFIX_SET for token in alpha_tokens):
        return False
    if any(token in banned for token in alpha_tokens):
        return False
    if max(len(token) for token in alpha_tokens) < 2:
        return False
    return sum(len(token) for token in alpha_tokens) >= 3


def classify_upload_address(value) -> tuple[bool, str]:
    if is_bad_addr(value):
        return False, "missing"
    text = normalize_upload_address(str(value))
    core = CITY_STATE_RE.sub("", text).strip(" ,")
    if not core:
        return False, "missing"
    if core.upper().startswith("UNRESOLVED_NON_SITE_MATTER_"):
        return False, "placeholder"
    if RELATIVE_RE.search(core):
        return False, "relative_or_non_site"
    if re.search(r"\bTHE\s+ONE\s+WAY\b", core, re.IGNORECASE):
        return False, "non_street_name"
    if STREET_ADDRESS_RE.fullmatch(core):
        street_tail = re.sub(r"^\d{1,6}(?:-\d{1,6})?\s+", "", core)
        if not street_name_quality_ok(street_tail, require_suffix=True):
            return False, "low_quality_street_name"
        return True, "street_address"
    if STREET_ADDRESS_SOFT_RE.fullmatch(core):
        street_tail = re.sub(r"^\d{1,6}(?:-\d{1,6})?\s+", "", core)
        if not street_name_quality_ok(street_tail, require_suffix=False):
            return False, "low_quality_street_name"
        return True, "street_address_soft"
    if INTERSECTION_RE.fullmatch(core):
        parts = re.split(r"\s+(?:AND|&)\s+", core, maxsplit=1, flags=re.IGNORECASE)
        if len(parts) != 2:
            return False, "not_street_pattern"
        if not street_name_quality_ok(parts[0], require_suffix=True) or not street_name_quality_ok(parts[1], require_suffix=True):
            return False, "low_quality_intersection_name"
        return True, "intersection"
    if INTERSECTION_SOFT_RE.fullmatch(core):
        parts = re.split(r"\s+(?:AND|&)\s+", core, maxsplit=1, flags=re.IGNORECASE)
        if len(parts) != 2:
            return False, "not_street_pattern"
        if not street_name_quality_ok(parts[0], require_suffix=False) or not street_name_quality_ok(parts[1], require_suffix=False):
            return False, "low_quality_intersection_name"
        return True, "intersection_soft"
    return False, "not_street_pattern"


def map_dash_address(title: str) -> str | None:
    parts = [normalize_chunk(part) for part in re.split(r"\s+-\s+", title) if normalize_chunk(part)]
    for part in parts[1:]:
        match = ADDR_RE.search(part)
        if match:
            return match.group(1)
    return None


def title_fallback_candidates(title: str | None) -> list[tuple[str, str]]:
    out: list[tuple[str, str]] = []
    if title is None or str(title).strip() == "" or str(title).lower() == "nan":
        return out
    text = str(title)

    bnd = BND_RE.search(text)
    if bnd:
        chunk = bnd.group(1)
        for piece in extract_address_chunks(chunk):
            out.append((f"{piece}, Chicago, IL", "matter_title_bounded_by_segment"))
        intersect = first_intersectionish(chunk)
        if intersect:
            out.append((f"{intersect}, Chicago, IL", "matter_title_bounded_by"))

    at = AT_RE.search(text)
    if at:
        chunk = at.group(1)
        for piece in extract_address_chunks(chunk):
            out.append((f"{piece}, Chicago, IL", "matter_title_at_segment"))
        normalized_chunk = normalize_chunk(chunk)
        if normalized_chunk:
            out.append((f"{normalized_chunk}, Chicago, IL", "matter_title_at"))

    dashed = map_dash_address(text)
    if dashed:
        out.append((f"{normalize_chunk(dashed)}, Chicago, IL", "matter_title_dash_segment"))

    generic = ADDR_RE.search(text)
    if generic:
        out.append((f"{normalize_chunk(generic.group(1))}, Chicago, IL", "matter_title_address_regex"))

    deduped: list[tuple[str, str]] = []
    seen = set()
    for candidate, source in out:
        key = normalize_chunk(candidate).upper()
        if key in seen:
            continue
        seen.add(key)
        deduped.append((candidate, source))
    return deduped


def main() -> int:
    args = parse_args()

    unmatched = pd.read_csv(args.in_unmatched_csv, dtype=str, low_memory=False)
    stage1 = pd.read_csv(args.in_stage1_csv, dtype=str, low_memory=False)

    base_cols = ["matter_id", "external_row_id", "matter_title"]
    metadata_candidates = [
        "matter_intro_date",
        "matter_passed_date",
        "record_source",
        "matter_file",
        "councilmatic_url",
        "councilmatic_pdf_url",
        "journal_year",
        "journal_meeting_date",
        "journal_filename",
        "journal_rel_local_path",
        "journal_pdf_url",
        "journal_page_number",
    ]
    metadata_cols = [col for col in metadata_candidates if col in stage1.columns]
    meta = stage1[base_cols + metadata_cols].copy()
    merged = unmatched.merge(meta, on=["matter_id", "external_row_id"], how="left")

    upload_address: list[str] = []
    source_used: list[str] = []
    placeholders: list[bool] = []
    validation_reasons: list[str] = []

    for _, row in merged.iterrows():
        candidate_pool: list[tuple[str, str]] = []
        addr = row.get("address_for_external")
        if not is_bad_addr(addr):
            candidate_pool.append((str(addr), "stage1_address_for_external"))
        candidate_pool.extend(title_fallback_candidates(row.get("matter_title")))

        chosen = None
        chosen_source = None
        chosen_reason = None
        first_reason = None
        for candidate, source in candidate_pool:
            normalized = normalize_upload_address(candidate)
            ok, reason = classify_upload_address(normalized)
            if first_reason is None:
                first_reason = reason
            if ok:
                chosen = normalized
                chosen_source = source
                chosen_reason = reason
                break

        if chosen is not None:
            upload_address.append(chosen)
            source_used.append(chosen_source or "unknown")
            placeholders.append(False)
            validation_reasons.append(chosen_reason or "ok")
            continue

        external_row_id = str(row.get("external_row_id", "")).strip() or "UNKNOWN"
        upload_address.append(f"UNRESOLVED_NON_SITE_MATTER_{external_row_id}")
        source_used.append("placeholder_non_site")
        placeholders.append(True)
        validation_reasons.append(first_reason or "no_candidate")

    merged["upload_address"] = upload_address
    merged["upload_source"] = source_used
    merged["is_placeholder_non_site"] = placeholders
    merged["upload_validation_reason"] = validation_reasons
    merged["upload_included"] = (~merged["is_placeholder_non_site"].fillna(False)) & (
        merged["upload_validation_reason"].isin(["street_address", "street_address_soft", "intersection", "intersection_soft"])
    )
    if "journal_pdf_url" in merged.columns:
        merged["source_pdf_url"] = merged["journal_pdf_url"]
    else:
        merged["source_pdf_url"] = pd.NA
    if "councilmatic_pdf_url" in merged.columns:
        merged["source_pdf_url"] = merged["source_pdf_url"].where(
            merged["source_pdf_url"].notna() & (merged["source_pdf_url"].astype(str).str.strip() != ""),
            merged["councilmatic_pdf_url"],
        )
    if "journal_meeting_date" in merged.columns:
        merged["source_date"] = merged["journal_meeting_date"]
    elif "matter_intro_date" in merged.columns:
        merged["source_date"] = merged["matter_intro_date"]
    else:
        merged["source_date"] = pd.NA
    if "matter_intro_date" in merged.columns:
        merged["source_date"] = merged["source_date"].where(
            merged["source_date"].notna() & (merged["source_date"].astype(str).str.strip() != ""),
            merged["matter_intro_date"],
        )
    if "matter_passed_date" in merged.columns:
        merged["source_date"] = merged["source_date"].where(
            merged["source_date"].notna() & (merged["source_date"].astype(str).str.strip() != ""),
            merged["matter_passed_date"],
        )

    merged["upload_row_number"] = pd.NA
    included_count = int(merged["upload_included"].sum())
    if included_count > 0:
        merged.loc[merged["upload_included"], "upload_row_number"] = range(1, included_count + 1)

    merged["address_occurrence"] = pd.NA
    if included_count > 0:
        included_occ = merged.loc[merged["upload_included"]].groupby("upload_address", dropna=False).cumcount() + 1
        merged.loc[merged["upload_included"], "address_occurrence"] = included_occ.values

    upload_df = (
        merged.loc[merged["upload_included"], ["upload_address"]]
        .rename(columns={"upload_address": "address"})
        .reset_index(drop=True)
    )

    ensure_parent(args.out_upload_xlsx)
    ensure_parent(args.out_mapping_csv)
    upload_df.to_excel(args.out_upload_xlsx, index=False)
    mapping_cols = [
        "upload_row_number",
        "address_occurrence",
        "external_row_id",
        "matter_id",
        "upload_address",
        "upload_source",
        "upload_validation_reason",
        "upload_included",
        "is_placeholder_non_site",
        "source_date",
        "source_pdf_url",
        "address_for_external",
        "matter_title",
    ]
    mapping_cols.extend(metadata_cols)
    # preserve order and avoid accidental duplicates
    ordered_cols: list[str] = []
    seen_cols = set()
    for col in mapping_cols:
        if col in merged.columns and col not in seen_cols:
            ordered_cols.append(col)
            seen_cols.add(col)
    merged[ordered_cols].to_csv(args.out_mapping_csv, index=False)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
