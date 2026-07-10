import argparse
import html
import io
import re
import time
from pathlib import Path

import pandas as pd
import requests
from pypdf import PdfReader
try:
    import pypdfium2 as pdfium
except Exception:  # noqa: BLE001
    pdfium = None
try:
    import pytesseract
except Exception:  # noqa: BLE001
    pytesseract = None

from title_parsers import parse_title_fields


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-matters", required=True)
    parser.add_argument("--in-sponsors", required=True)
    parser.add_argument("--in-histories", required=True)
    parser.add_argument("--in-attachments", required=True)
    parser.add_argument("--in-pdf-fields", required=True)
    parser.add_argument("--in-pdf-text")
    parser.add_argument("--in-candidates", required=True)
    parser.add_argument("--in-councilmatic-fields", required=True)
    parser.add_argument("--sample-start-date", required=True)
    parser.add_argument("--sample-end-date", required=True)
    parser.add_argument("--out-csv", required=True)
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def choose_column(df: pd.DataFrame, options: list[str]) -> str | None:
    for col in options:
        if col in df.columns:
            return col
    return None


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


def normalize_matter_id(value) -> str | None:
    if value is None or pd.isna(value):
        return None
    text = str(value).strip()
    if not text:
        return None
    return text


def first_non_null(series: pd.Series):
    for value in series:
        if pd.notna(value) and value not in (""):
            return value
    return None


COUNCILMATIC_DETAIL_URL = "https://chicago.councilmatic.org/legislation/{slug}/"
COUNCILMATIC_TEXT_RE = re.compile(r'"text"\s*:\s*"(.*)"\s*\n\s*}\s*\n\s*</script>', re.S)
COUNCILMATIC_PDF_LINK_RE = re.compile(
    r"<a[^>]+id=['\"]pdf-download-link['\"][^>]+href=['\"]([^'\"]+)['\"]",
    re.IGNORECASE,
)
COUNCILMATIC_FIELD_COLUMNS = [
    "matter_id",
    "matter_file",
    "councilmatic_url",
    "councilmatic_pdf_url",
    "councilmatic_status",
    "councilmatic_text",
    "councilmatic_text_chars",
    "councilmatic_from_zoning",
    "councilmatic_to_zoning",
]
ORDINANCE_CUTOFF_PATTERNS = [
    re.compile(r"\bAPPLICATION\s+FOR\s+AN\s+AMENDMENT\s+TO\s+THE\s+CHICAGO\s+ZONING\s+ORDINANCE\b", re.IGNORECASE),
    re.compile(r"\bCITY\s+OF\s+CHICAGO\s+APPLICATION\s+FOR\s+AN\s+AMENDMENT\b", re.IGNORECASE),
    re.compile(r"\bTYPE\s+1\s+ZONING\s+AMENDMENT\b", re.IGNORECASE),
    re.compile(r"\bZONING\s+AND\s+DEVELOPMENT\s+NARRATIVE\b", re.IGNORECASE),
]
ZONE_END_PATTERNS = [
    re.compile(r"\bsection\s*2\b", re.IGNORECASE),
    re.compile(r"\bthis ordinance\b", re.IGNORECASE),
    re.compile(r"\bcommon address\b", re.IGNORECASE),
    re.compile(r"\baddress range\b", re.IGNORECASE),
]
TO_THOSE_OF_PATTERN = (
    r"\b(?:to|lo)[\s'`\",.;:-]*(?:those|that|lho\.?se)[\s'`\",.;:-]*"
    r"(?:of|ot|0f|o[!l1])[\s'`\",.;:-]*(?:a|an)?\s*"
)
TO_THOSE_OF_RE = re.compile(TO_THOSE_OF_PATTERN, re.IGNORECASE)
FROM_TO_PATTERNS = [
    re.compile(
        r"\bchanging\s+properties\s+currently\s+zoned\s+as\s+(?P<from>.+?)\s+in\s+the\s+area\s+bounded\b"
        rf".*?{TO_THOSE_OF_PATTERN}(?P<to>.+?)"
        r"(?:\.\s*(?:section\s*2|this ordinance|common address)\b|\bsection\s*2\b|[.;])",
        re.IGNORECASE | re.DOTALL,
    ),
    re.compile(
        r"\bcurrent\s+zoning\s+designation\s+of\s+(?P<from>(?:(?!\bbe\s+changed\b).)+?)\s+to\s+(?:that\s+of\s+)?"
        r"(?:a\s+|an\s+)?(?P<to>.+?)(?:\s+to\s+allow\b|\s+for\b|\s+section\s*2\b|\s+this ordinance\b|[.;])",
        re.IGNORECASE | re.DOTALL,
    ),
    re.compile(
        r"\bcurrent\s+zoning\s+designation\s+of\s+(?P<from>.+?)\s+be\s+changed\s+to\s+"
        r"(?P<to>.+?)(?:\s+to\s+allow\b|\s+for\b|\s+section\s*2\b|\s+this ordinance\b|[.;])",
        re.IGNORECASE | re.DOTALL,
    ),
    re.compile(
        r"changing(?:\s+all)?(?:\s+of)?(?:\s+the)?\s+(?P<from>.+?)\s+symbols?(?:\s+and\s+indications?)?"
        rf".*?{TO_THOSE_OF_PATTERN}(?P<to>.+?)"
        r"(?:\.\s*(?:section\s*2|this ordinance|common address)\b|\bsection\s*2\b|[.;])",
        re.IGNORECASE | re.DOTALL,
    ),
    re.compile(
        r"\bzoning\s+change\s+from\s+(?:an?\s+|the\s+current\s+)?(?P<from>.+?)\s+to\s+"
        r"(?:those?\s+of\s+)?(?:an?\s+|the\s+current\s+)?(?P<to>.+?)"
        r"(?:\s+for\b|\s+section\s*2\b|\s+this ordinance\b|[.;])",
        re.IGNORECASE | re.DOTALL,
    ),
    re.compile(
        r"\bfrom\s+(?:the\s+current\s+|an?\s+)?(?P<from>[A-Z0-9][A-Za-z0-9\-\s,./()#&]+?)\s+"
        r"(?:to|into)\s+(?:those?\s+of\s+)?(?:the\s+current\s+|an?\s+)?"
        r"(?P<to>[A-Z0-9][A-Za-z0-9\-\s,./()#&]+?)(?:\s+for\b|\s+section\s*2\b|\s+this ordinance\b|[.;])",
        re.IGNORECASE | re.DOTALL,
    ),
]
ZONE_CODE_RE = re.compile(r"\b[A-Z]{1,3}\d?(?:\s*-\s*|\s+)?\d(?:\.\d+)?\b", re.IGNORECASE)
ZONE_INDICATOR_RE = re.compile(r"(?:\bdistrict\b|\bplanned development\b)", re.IGNORECASE)
ZONE_DESCRIPTOR_RE = re.compile(
    r"(?:neighborhood|community shopping|mixed-use|residential|commercial|manufacturing|industry|"
    r"business park|motor vehicle|downtown|parks|open space|transportation)",
    re.IGNORECASE,
)
ZONE_NOISE_PATTERNS = [
    re.compile(r"\bin order to\b.*$", re.IGNORECASE),
    re.compile(r"\bto allow\b.*$", re.IGNORECASE),
    re.compile(r"\bto permit\b.*$", re.IGNORECASE),
    re.compile(r"\bto develop\b.*$", re.IGNORECASE),
    re.compile(r"\bfor the purposes?\b.*$", re.IGNORECASE),
    re.compile(r"\bfor purposes?\b.*$", re.IGNORECASE),
    re.compile(r"\bwhich is hereby\b.*$", re.IGNORECASE),
    re.compile(r"\bas amended\b.*$", re.IGNORECASE),
    re.compile(r"\bin the area\b.*$", re.IGNORECASE),
    re.compile(r"\bat the subject property\b.*$", re.IGNORECASE),
    re.compile(r"\bsymbols?\s+and\s+indications?.*$", re.IGNORECASE),
    re.compile(r"\bsection\s*2\b.*$", re.IGNORECASE),
    re.compile(r"\bsection\s*3\b.*$", re.IGNORECASE),
    re.compile(r"\bthis ordinance\b.*$", re.IGNORECASE),
    re.compile(r"\bcommon address\b.*$", re.IGNORECASE),
    re.compile(r"\bproject narrative\b.*$", re.IGNORECASE),
    re.compile(r"\bthis change of zoning classification\b.*$", re.IGNORECASE),
    re.compile(r"\btriggered the requirements?\b.*$", re.IGNORECASE),
    re.compile(r"\brezoning process\b.*$", re.IGNORECASE),
    re.compile(r"\baffordable requirements ordinance\b.*$", re.IGNORECASE),
    re.compile(r"\band a corresponding use district is hereby established\b.*$", re.IGNORECASE),
    re.compile(r"\bthen\b\s*$", re.IGNORECASE),
]
OCR_RENDER_SCALE = 2.5
OCR_MAX_PAGES = 40
OCR_READY = pdfium is not None and pytesseract is not None
ALLOWED_ZONE_PREFIXES = {
    "R",
    "RS",
    "RT",
    "RM",
    "B1",
    "B2",
    "B3",
    "B4",
    "B5",
    "B6",
    "B7",
    "C1",
    "C2",
    "C3",
    "C4",
    "C5",
    "M1",
    "M2",
    "M3",
    "DR",
    "DX",
    "DC",
    "DS",
    "PMD",
    "PMO",
    "CL",
    "CI",
    "BL",
    "ML",
}
TITLE_MAP_RECLASS_PATTERNS = [
    re.compile(r"\bzoning\s+reclass\w*\b.*\bmap(?:s)?\b.*\bno\b", re.IGNORECASE),
    re.compile(r"\bzoning\s+reclass\w*\b\s+on\s+map\b", re.IGNORECASE),
    re.compile(r"\breclassification\b.*\bmap(?:s)?\b.*\bno\b", re.IGNORECASE),
]


def matter_file_to_slug(matter_file: str | None) -> str | None:
    if not matter_file:
        return None
    slug = str(matter_file).strip().lower()
    slug = re.sub(r"[^a-z0-9\-]", "", slug)
    return slug or None


def clean_text(value: str | None) -> str | None:
    if value is None:
        return None
    out = re.sub(r"\s+", " ", str(value)).strip(" .;:\t\r\n")
    return out or None


def normalize_numeric_token(value: str | None) -> str:
    if not value:
        return ""
    out = str(value).upper().strip()
    return out.replace("I", "1").replace("L", "1").replace("O", "0").replace("S", "5").replace("$", "5")


def is_plausible_zoning_code(code: str | None) -> bool:
    if not code:
        return False
    value = str(code).upper().strip()
    if value in {"PD", "POS", "T", "PMD", "PMO"}:
        return True
    if re.fullmatch(r"R[1-8]", value):
        return True
    if re.fullmatch(r"[BCM]\d", value):
        return True
    if "-" not in value:
        return False
    prefix, suffix = value.split("-", 1)
    if prefix not in ALLOWED_ZONE_PREFIXES:
        return False
    numeric = re.match(r"^(\d{1,2})(?:\.(\d))?[A-Z]?$", suffix)
    if not numeric:
        return False
    major = int(numeric.group(1))
    return major <= 20


def preprocess_ordinance_text(text: str | None, truncate_forms: bool) -> str | None:
    if not text:
        return None

    out = str(text).replace("\u00a0", " ")
    out = re.sub(r"\bofthe\b", "of the", out, flags=re.IGNORECASE)
    out = re.sub(r"\ballthe\b", "all the", out, flags=re.IGNORECASE)
    out = re.sub(r"\btlie\b", "the", out, flags=re.IGNORECASE)
    out = re.sub(
        r"([A-Za-z])(?=to\s+(?:those|that)\s+(?:of|ot|0f)\b)",
        r"\1 ",
        out,
        flags=re.IGNORECASE,
    )
    out = re.sub(r"\bto\s+(?:those|that)\s+ot\b", "to those of", out, flags=re.IGNORECASE)
    out = re.sub(r"\bto\s+(?:those|that)\s+0f\b", "to those of", out, flags=re.IGNORECASE)
    out = re.sub(r"\blo\s+(?:those|that)\s+(?:of|ot|0f|o[!l1])\b", "to those of", out, flags=re.IGNORECASE)
    out = re.sub(r"\bto\s+lho\.?se\s+(?:of|ot|0f|o[!l1])\b", "to those of", out, flags=re.IGNORECASE)
    out = re.sub(r"\bto\s+(?:those|that)\s+o[!l1]\b", "to those of", out, flags=re.IGNORECASE)
    out = re.sub(r"\bBZ\b(?=[\s\"'\[]+Neighborhood\s+Mixed-Use)", "B2-1", out, flags=re.IGNORECASE)
    out = re.sub(r"\s+", " ", out).strip()

    if truncate_forms:
        cut_idx = len(out)
        for pattern in ORDINANCE_CUTOFF_PATTERNS:
            match = pattern.search(out)
            if match and match.start() > 120:
                cut_idx = min(cut_idx, match.start())
        if cut_idx < len(out):
            out = out[:cut_idx].strip()

    return out or None


def parse_changing_symbols(text: str) -> tuple[str | None, str | None]:
    start = re.search(r"\bchanging(?:\s+all)?(?:\s+of)?(?:\s+the)?\s+", text, re.IGNORECASE)
    if not start:
        return None, None

    zone_delimiter = re.search(
        r"\b(?:symbols?|syrnbols?|indications?)\b|(?=,?\s*(?:as\s+shown|shown)\s+on\s+map\b)",
        text[start.end() :],
        re.IGNORECASE,
    )
    if not zone_delimiter:
        return None, None

    from_raw = text[start.end() : start.end() + zone_delimiter.start()]
    tail = text[start.end() + zone_delimiter.end() :]
    to_matches = list(TO_THOSE_OF_RE.finditer(tail))
    if not to_matches:
        return normalize_zone_value(from_raw), None

    to_tail = tail[to_matches[-1].end() :]
    cut_idx = len(to_tail)
    for pattern in ZONE_END_PATTERNS:
        match = pattern.search(to_tail)
        if match:
            cut_idx = min(cut_idx, match.start())
    to_raw = to_tail[:cut_idx]

    return normalize_zone_value(from_raw), normalize_zone_value(to_raw)


def normalize_zone_value(value: str | None) -> str | None:
    out = clean_text(value)
    if not out:
        return None
    out = out.replace("\u2013", "-").replace("\u2014", "-")
    out = re.sub(r"^\s*[NA]\s+(?=[A-Z0-9])", "", out, flags=re.IGNORECASE)
    out = re.sub(r"\b33-(\d)\b", r"B3-\1", out, flags=re.IGNORECASE)
    out = re.sub(r"\b83-(\d)\b", r"B3-\1", out, flags=re.IGNORECASE)
    out = re.sub(r"\bBZ-(\d)\b", r"B2-\1", out, flags=re.IGNORECASE)
    out = re.sub(r"\bBZ\b(?=[\s\"'\[]+Neighborhood\s+Mixed-Use)", "B2-1", out, flags=re.IGNORECASE)
    out = re.sub(r"\bBZ\b", "B2", out, flags=re.IGNORECASE)
    out = re.sub(r"\b([A-Z]{1,4}\d?-\d+(?:\.\d+)?)[\"']", r"\1", out, flags=re.IGNORECASE)
    out = re.sub(r"\bRT#\b", "RT-3", out, flags=re.IGNORECASE)
    out = re.sub(r"\bRS#\b", "RS-3", out, flags=re.IGNORECASE)
    out = re.sub(r"\bRT\\\$\b", "RT-3.5", out, flags=re.IGNORECASE)
    out = re.sub(r"\bRT\$\b", "RT-3.5", out, flags=re.IGNORECASE)
    out = re.sub(r"\b([A-Z]{1,4})(\d)\.(\d)\b", r"\1\2-\3", out, flags=re.IGNORECASE)
    out = re.sub(r"^to those of (?:a|an)\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^that of (?:a|an)\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^of\s+the\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^ofthe\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^current\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^its\s+current\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^district\s+(?=[A-Z0-9])", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^boundaries of the\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^(?:the|a|an)\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"\b([A-Za-z]{1,3})(\d(?:\.\d+)?)(?!\s*[-\d])\b", r"\1-\2", out)
    out = re.sub(r"\b([A-Za-z]{1,3}\d?)\s*-\s*(\d(?:\.\d+)?)\b", r"\1-\2", out)
    out = re.sub(r"\b([A-Za-z]{1,3}\d?)\s+(\d(?:\.\d+)?)\b", r"\1-\2", out)
    out = re.sub(r"\b([A-Za-z]{1,3}-\d(?:\.\d+)?)\b", lambda m: m.group(1).upper(), out)
    for pattern in ZONE_NOISE_PATTERNS:
        out = pattern.sub("", out).strip()
    out = re.sub(r"\band\s*$", "", out, flags=re.IGNORECASE).strip()
    out = out.strip(" .;:,")
    if not out:
        return None
    if len(out.split()) > 28 and not ZONE_INDICATOR_RE.search(out):
        return None
    if re.search(
        r"\b(owner|applicant|authorization|proceed|committee on|meeting date|document tracking sheet|city clerk)\b",
        out,
        re.IGNORECASE,
    ):
        return None
    if not ZONE_INDICATOR_RE.search(out) and not ZONE_CODE_RE.search(out) and not ZONE_DESCRIPTOR_RE.search(out):
        return None
    return out


def has_ordered_code_sequence_context(text: str | None) -> bool:
    normalized = clean_text(text)
    if not normalized:
        return False
    return bool(
        re.search(
            r"\bchanging\b|\bcurrent\s+zoning\s+designation\b|\bzoning\s+change\s+from\b|\brezone\s+from\b|\brezoned\s+from\b",
            normalized,
            re.IGNORECASE,
        )
    )


def parse_from_to_zoning(text: str | None) -> tuple[str | None, str | None]:
    if not text:
        return None, None

    candidates: list[str] = []
    for truncate_forms in [True, False]:
        candidate = preprocess_ordinance_text(text, truncate_forms=truncate_forms)
        if candidate and candidate not in candidates:
            candidates.append(candidate)

    partial = None
    partial_quality = 0
    for candidate in candidates:
        from_zone, to_zone = parse_changing_symbols(candidate)
        pair_quality = parsed_pair_quality(from_zone, to_zone)
        if pair_quality >= 2:
            return from_zone, to_zone
        if pair_quality > partial_quality:
            partial = (from_zone, to_zone)
            partial_quality = pair_quality

        for pattern in FROM_TO_PATTERNS:
            for match in pattern.finditer(candidate):
                from_zone = normalize_zone_value(match.group("from"))
                to_zone = normalize_zone_value(match.group("to"))
                pair_quality = parsed_pair_quality(from_zone, to_zone)
                if pair_quality >= 2:
                    return from_zone, to_zone
                if pair_quality > partial_quality:
                    partial = (from_zone, to_zone)
                    partial_quality = pair_quality

    partial_from = partial[0] if partial else None
    partial_to = partial[1] if partial else None

    # OCR fallback: when one side is missing, recover pair from ordered code sequence in text.
    sequence_source = " ".join(candidates)
    sequence_codes = extract_canonical_code_sequence(sequence_source)
    if sequence_codes and any(has_ordered_code_sequence_context(candidate) for candidate in candidates):
        partial_from_code = parse_zoning_code_for_quality(partial_from) if partial_from else None
        partial_to_code = parse_zoning_code_for_quality(partial_to) if partial_to else None

        if partial_from_code and not partial_to_code:
            alt = next((code for code in sequence_codes if code != partial_from_code), None)
            if alt:
                return partial_from, alt
        if partial_to_code and not partial_from_code:
            alt = next((code for code in sequence_codes if code != partial_to_code), None)
            if alt:
                return alt, partial_to
        if len(sequence_codes) >= 2:
            return sequence_codes[0], sequence_codes[1]

    if partial_quality > 0:
        return partial
    return None, None


def parse_quality(from_zone: str | None, to_zone: str | None) -> int:
    if from_zone and to_zone:
        return 2
    if from_zone or to_zone:
        return 1
    return 0


def parse_zoning_code_for_quality(value: str | None) -> str | None:
    text = clean_text(value)
    if not text:
        return None
    text = text.upper().replace("\u2013", "-").replace("\u2014", "-")
    text = text.replace("-L", "-1").replace("-I", "-1")
    text = text.replace("BL-I", "BL-1").replace("BI-I", "BI-1").replace("ML-I", "ML-1")
    text = text.replace("M1-I", "M1-1").replace("M2-I", "M2-1").replace("M3-I", "M3-1")
    text = text.replace("B1-I", "B1-1").replace("B2-I", "B2-1").replace("B3-I", "B3-1")
    text = re.sub(r"\b33-(\d)\b", r"B3-\1", text)
    text = re.sub(r"\b83-(\d)\b", r"B3-\1", text)
    text = re.sub(r"\bBZ-(\d)\b", r"B2-\1", text)
    text = re.sub(r"\bBZ\b(?=[\s\"'\[]+NEIGHBORHOOD\s+MIXED-USE)", "B2-1", text)
    text = re.sub(r"\bBZ\b", "B2", text)
    text = re.sub(r"\bBI-(\d)\b", r"B1-\1", text)
    text = re.sub(r"\bCI-(\d)\b", r"C1-\1", text)
    text = re.sub(r"\b0([1-5])-(\d(?:\.\d+)?)\b", r"C\1-\2", text)
    text = re.sub(r"\bRMS\.?5\b", "RM-5.5", text)
    text = re.sub(r"\bRMS\b", "RM-5", text)
    text = re.sub(r"\bRM-S\b", "RM-5", text)
    text = re.sub(r"\bRSI\b", "RS-1", text)
    text = re.sub(r"\bRTS\.5\b", "RT-3.5", text)
    text = re.sub(r"\bRT#\b", "RT-3", text)
    text = re.sub(r"\bRS#\b", "RS-3", text)
    text = text.replace("$", "S").replace("~", "-")
    text = re.sub(r"\bR\s*1\s*([34])[\.\-]?\s*5\b", r"RT-\1.5", text)
    text = re.sub(r"\b(RS|RT|RM|DR|DX|DC|DS)\s*([0-9]+(?:\.[0-9]+)?)\b", r"\1-\2", text)
    text = re.sub(r"\b(RS|RT|RM)\s*-\s*(\d)\s*-\s*(\d)\b", r"\1-\2.\3", text)
    text = re.sub(r"\b([A-Z]{1,4})(\d)\.(\d)\b", r"\1\2-\3", text)
    text = re.sub(r"\b([R][SMT])(\d)-(\d)\b", r"\1-\2.\3", text)
    text = re.sub(r"\b([BCM])-(\d)\b", r"\1\2", text)
    text = re.sub(r"\b([BCM])\s+(\d)\b", r"\1\2", text)
    text = re.sub(r"\s+", " ", text).strip()

    if "PLANNED DEVELOPMENT" in text or re.search(r"\b(?:PD|RBPD|BPD|RPD|IPD)\b", text):
        return "PD"
    if re.search(r"\bPOS\b", text) or "OPEN SPACE" in text:
        return "POS"
    if text == "T" or text.startswith("T-") or text.startswith("T "):
        return "T"

    pmd = re.search(r"\bPMD\s*(?:#|NO\.?)?\s*([0-9ILSO]+)?\b", text)
    if pmd:
        num = normalize_numeric_token(pmd.group(1))
        return f"PMD-{num}" if num else "PMD"

    old_r = re.search(r"\bR([1-8])\b", text)
    if old_r:
        return f"R{old_r.group(1)}"

    text = re.sub(r"\b([A-Z]{1,4}\d?)\s+(\d+(?:\.\d+)?)\b", r"\1-\2", text)
    text = re.sub(r"\b([A-Z]{1,4})\s*-\s*(\d)\s*-\s*(\d+(?:\.\d+)?[A-Z]?)\b", r"\1\2-\3", text)

    match = re.search(r"\b([A-Z]{1,4}\d?-\d{1,2}(?:\.\d)?[A-Z]?)\b", text)
    if match:
        code = match.group(1)
        if is_plausible_zoning_code(code):
            return code

    c4 = re.search(r"\bC\s*[- ]?4\b", text)
    if c4:
        return "C4"
    return None


def extract_canonical_code_sequence(text: str | None) -> list[str]:
    if not text:
        return []
    working = clean_text(text)
    if not working:
        return []
    working = working.upper().replace("\u2013", "-").replace("\u2014", "-")
    working = re.sub(r"\s+", " ", working)
    changing_match = re.search(r"\bCHANGING\b", working)
    if changing_match:
        working = working[changing_match.start() :]
    cut_match = re.search(r"\b(IN THE AREA BOUNDED BY|AREA BOUNDED BY|COMMON ADDRESS|SECTION\s*2|SECTION\s*3)\b", working)
    if cut_match:
        working = working[: cut_match.start()]
    if len(working) > 900:
        working = working[:900]

    found: list[str] = []

    def add(code: str | None) -> None:
        if code and code not in found:
            found.append(code)

    for part in re.split(r"\s+(?:AND|&)\s+|,|/|\(|\)", working):
        add(parse_zoning_code_for_quality(part))

    for match in re.finditer(
        r"\b(?:PLANNED DEVELOPMENT|RBPD|BPD|RPD|IPD|POS)\b|\bT\b(?=\s*(?:,|-|\s)TRANSPORTATION|\s+DISTRICT\b)",
        working,
    ):
        add(parse_zoning_code_for_quality(match.group(0)))

    for match in re.finditer(r"\b([A-Z]{1,4}\d?)\s*[- ]?\s*([0-9ILSO]{1,2}(?:\.[0-9ILSO])?)([A-Z]?)\b", working):
        token = f"{match.group(1)}-{match.group(2)}{match.group(3)}"
        add(parse_zoning_code_for_quality(token))

    return found


def parsed_pair_quality(from_zone: str | None, to_zone: str | None) -> int:
    from_ok = parse_zoning_code_for_quality(from_zone) is not None
    to_ok = parse_zoning_code_for_quality(to_zone) is not None
    if from_ok and to_ok:
        return 2
    if from_ok or to_ok:
        return 1
    return 0


def has_map_reclassification_title(value: str | None) -> bool:
    text = clean_text(value)
    if not text:
        return False
    normalized = text.replace("\u2013", "-").replace("\u2014", "-")
    normalized = re.sub(r"[,:;]", " ", normalized)
    normalized = re.sub(r"\s+", " ", normalized).strip()
    return any(pattern.search(normalized) for pattern in TITLE_MAP_RECLASS_PATTERNS)


def is_non_base_zoning_action(text: str | None) -> bool:
    normalized = clean_text(text)
    if not normalized:
        return False
    sequence_codes = [code for code in extract_canonical_code_sequence(normalized) if code != "T"]
    has_base_change_phrase = bool(re.search(r"\bchanging\b.+\bto\s+(?:those|that|the)\b", normalized, re.IGNORECASE | re.DOTALL))
    if sequence_codes:
        return False
    if re.search(r"\bpedestrian\s+(?:retail\s+)?street\b", normalized, re.IGNORECASE) and re.search(
        r"\bremov(?:e|es|ing)\b",
        normalized,
        re.IGNORECASE,
    ):
        return True
    if (
        re.search(r"\bplat\s+of\s+vacation\b|\bvacated\s+and\s+closed\b|\bpublic\s+way\s+vacat", normalized, re.IGNORECASE)
        and not has_base_change_phrase
    ):
        return True
    return False


def extract_councilmatic_text(page_html: str) -> str | None:
    match = COUNCILMATIC_TEXT_RE.search(page_html)
    if not match:
        return None
    text = html.unescape(match.group(1))
    text = text.replace("\\r", "\r").replace("\\n", "\n").strip()
    return text or None


def extract_councilmatic_pdf_url(page_html: str) -> str | None:
    match = COUNCILMATIC_PDF_LINK_RE.search(page_html)
    if not match:
        return None
    return html.unescape(match.group(1)).strip() or None


def extract_pdf_text_ocr(content: bytes, max_pages: int = OCR_MAX_PAGES) -> str | None:
    if not OCR_READY:
        return None
    try:
        document = pdfium.PdfDocument(content)
    except Exception:  # noqa: BLE001
        return None

    page_total = len(document)
    if page_total <= 0:
        return None

    parts: list[str] = []
    page_limit = min(page_total, max_pages)
    for idx in range(page_limit):
        try:
            page = document[idx]
            bitmap = page.render(scale=OCR_RENDER_SCALE)
            image = bitmap.to_pil()
            text = pytesseract.image_to_string(image) or ""
        except Exception:  # noqa: BLE001
            text = ""
        finally:
            try:
                image.close()
            except Exception:  # noqa: BLE001
                pass
            try:
                page.close()
            except Exception:  # noqa: BLE001
                pass
        if text.strip():
            parts.append(text)

    out = "\n".join(parts).strip()
    return out or None


def extract_pdf_text_bytes(content: bytes) -> tuple[str | None, bool]:
    if not content:
        return None, False
    if not content.startswith(b"%PDF"):
        return None, False
    try:
        reader = PdfReader(io.BytesIO(content))
    except Exception:  # noqa: BLE001
        return None, False

    parts: list[str] = []
    for page in reader.pages:
        try:
            page_text = page.extract_text() or ""
        except Exception:  # noqa: BLE001
            page_text = ""
        if page_text:
            parts.append(page_text)

    text = "\n".join(parts).strip()
    if text:
        return text, False

    ocr_text = extract_pdf_text_ocr(content)
    if ocr_text:
        return ocr_text, True
    return None, False


def councilmatic_target_rows(matters: pd.DataFrame) -> pd.DataFrame:
    if matters.empty:
        return pd.DataFrame(columns=["matter_id", "matter_file"])

    parse_quality_values = matters.apply(
        lambda row: parsed_pair_quality(row.get("from_zoning"), row.get("to_zoning")),
        axis=1,
    )
    target = matters[
        parse_quality_values.lt(2)
        & matters["matter_id"].notna()
        & matters["matter_file"].notna()
    ][["matter_id", "matter_file"]].drop_duplicates()
    return target.sort_values("matter_id").reset_index(drop=True)


def normalize_councilmatic_fields(df: pd.DataFrame, required_targets: pd.DataFrame) -> pd.DataFrame:
    missing_cols = [col for col in COUNCILMATIC_FIELD_COLUMNS if col not in df.columns]
    if missing_cols:
        raise ValueError(f"Councilmatic fallback input is missing columns: {missing_cols}")

    out = df[COUNCILMATIC_FIELD_COLUMNS].copy()
    out["matter_id"] = out["matter_id"].map(normalize_matter_id)
    out = out[out["matter_id"].notna()].copy()

    duplicates = out.loc[out["matter_id"].duplicated(keep=False), "matter_id"].dropna().unique()
    if len(duplicates) > 0:
        sample = ", ".join(sorted(duplicates)[:10])
        raise ValueError(f"Councilmatic fallback input has duplicate matter_id rows: {sample}")

    required_ids = set(required_targets["matter_id"].dropna())
    present_ids = set(out["matter_id"].dropna())
    missing_ids = sorted(required_ids - present_ids)
    if missing_ids:
        sample = ", ".join(missing_ids[:20])
        raise ValueError(f"Councilmatic fallback input is missing required matter_id rows: {sample}")

    return out


def fetch_councilmatic_fields(
    matters: pd.DataFrame,
    sleep_seconds: float,
    timeout_seconds: int = 60,
    progress_every: int = 0,
) -> pd.DataFrame:
    target = councilmatic_target_rows(matters)

    rows = []
    if target.empty:
        return pd.DataFrame(
            columns=[
                "matter_id",
                "matter_file",
                "councilmatic_url",
                "councilmatic_pdf_url",
                "councilmatic_status",
                "councilmatic_text",
                "councilmatic_text_chars",
                "councilmatic_from_zoning",
                "councilmatic_to_zoning",
            ]
        )

    session = requests.Session()
    for row_number, (_, rec) in enumerate(target.iterrows(), start=1):
        matter_id = rec["matter_id"]
        matter_file = rec["matter_file"]
        if progress_every > 0 and (row_number == 1 or row_number % progress_every == 0):
            print(f"fetch_councilmatic {row_number}/{len(target)} {matter_id}", flush=True)
        slug = matter_file_to_slug(matter_file)
        if not slug:
            rows.append(
                {
                    "matter_id": matter_id,
                    "matter_file": matter_file,
                    "councilmatic_url": None,
                    "councilmatic_pdf_url": None,
                    "councilmatic_status": "missing_slug",
                    "councilmatic_text": None,
                    "councilmatic_text_chars": None,
                    "councilmatic_from_zoning": None,
                    "councilmatic_to_zoning": None,
                }
            )
            continue

        url = COUNCILMATIC_DETAIL_URL.format(slug=slug)
        status = "error"
        text = None
        pdf_url = None
        from_zone = None
        to_zone = None

        try:
            response = session.get(url, timeout=timeout_seconds)
            if response.status_code == 404:
                status = "not_found"
            elif response.status_code >= 400:
                status = f"http_{response.status_code}"
            else:
                pdf_url = extract_councilmatic_pdf_url(response.text)
                text = extract_councilmatic_text(response.text)
                if text:
                    status = "ok"
                    from_zone, to_zone = parse_from_to_zoning(text)
                    # Some early records only expose useful text in linked PDFs.
                    if parse_quality(from_zone, to_zone) < 2 and pdf_url:
                        try:
                            pdf_response = session.get(pdf_url, timeout=timeout_seconds)
                            if pdf_response.status_code < 400:
                                pdf_text, used_ocr = extract_pdf_text_bytes(pdf_response.content)
                                if pdf_text:
                                    pdf_from, pdf_to = parse_from_to_zoning(pdf_text)
                                    if parse_quality(pdf_from, pdf_to) > parse_quality(from_zone, to_zone):
                                        from_zone = pdf_from
                                        to_zone = pdf_to
                                        status = "ok_pdf_backfill_ocr" if used_ocr else "ok_pdf_backfill"
                                    if len(pdf_text) > len(text):
                                        text = pdf_text
                            elif status == "ok":
                                status = f"ok_pdf_http_{pdf_response.status_code}"
                        except Exception:  # noqa: BLE001
                            if status == "ok":
                                status = "ok_pdf_error"
                else:
                    if not pdf_url:
                        status = "no_text"
                    else:
                        try:
                            pdf_response = session.get(pdf_url, timeout=timeout_seconds)
                            if pdf_response.status_code >= 400:
                                status = f"pdf_http_{pdf_response.status_code}"
                            else:
                                text, used_ocr = extract_pdf_text_bytes(pdf_response.content)
                                if text:
                                    status = "ok_pdf_ocr" if used_ocr else "ok_pdf"
                                    from_zone, to_zone = parse_from_to_zoning(text)
                                else:
                                    status = "pdf_no_text"
                        except Exception:  # noqa: BLE001
                            status = "pdf_error"
        except Exception:  # noqa: BLE001
            status = "error"

        rows.append(
            {
                "matter_id": matter_id,
                "matter_file": matter_file,
                "councilmatic_url": url,
                "councilmatic_pdf_url": pdf_url,
                "councilmatic_status": status,
                "councilmatic_text": text,
                "councilmatic_text_chars": len(text) if text else None,
                "councilmatic_from_zoning": from_zone,
                "councilmatic_to_zoning": to_zone,
            }
        )

        if sleep_seconds > 0:
            time.sleep(sleep_seconds)

    return pd.DataFrame(rows)


def normalize_candidates(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if "matter_id" not in out.columns and "MatterId" in out.columns:
        out["matter_id"] = out["MatterId"]

    out["matter_id"] = out["matter_id"].map(normalize_matter_id)

    if "is_final_candidate" not in out.columns:
        out["is_final_candidate"] = True
    else:
        out["is_final_candidate"] = to_bool_series(out["is_final_candidate"])

    if "zoning_class" not in out.columns:
        out["zoning_class"] = None

    return out


def normalize_matters(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out["matter_id"] = out[choose_column(out, ["matter_id", "MatterId"])].map(normalize_matter_id)

    out["matter_file"] = out[choose_column(out, ["matter_file", "MatterFile"])]
    out["matter_title"] = out[choose_column(out, ["matter_title", "MatterTitle"])]
    out["matter_status_name"] = out[choose_column(out, ["matter_status_name", "MatterStatusName"])]
    out["matter_body_name"] = out[choose_column(out, ["matter_body_name", "MatterBodyName"])]
    out["matter_intro_date"] = out[choose_column(out, ["matter_intro_date", "MatterIntroDate"])]
    out["matter_intro_date_raw"] = out[choose_column(out, ["matter_intro_date_raw"])] if "matter_intro_date_raw" in out.columns else None
    out["matter_passed_date"] = out[choose_column(out, ["matter_passed_date", "MatterPassedDate"])]
    out["matter_guid"] = out[choose_column(out, ["matter_guid"])] if "matter_guid" in out.columns else None
    out["source_system"] = out[choose_column(out, ["source_system"])] if "source_system" in out.columns else None
    if "date_imputed_from_final_action" in out.columns:
        out["date_imputed_from_final_action"] = to_bool_series(out["date_imputed_from_final_action"])
    else:
        out["date_imputed_from_final_action"] = False

    out["matter_intro_date"] = pd.to_datetime(out["matter_intro_date"], errors="coerce", utc=True).dt.tz_convert(None)
    out["matter_passed_date"] = pd.to_datetime(out["matter_passed_date"], errors="coerce", utc=True).dt.tz_convert(None)

    keep = [
        "matter_id",
        "matter_guid",
        "matter_file",
        "matter_title",
        "matter_status_name",
        "matter_body_name",
        "source_system",
        "date_imputed_from_final_action",
        "matter_intro_date_raw",
        "matter_intro_date",
        "matter_passed_date",
    ]
    return out[keep].drop_duplicates(subset=["matter_id"]).reset_index(drop=True)


def normalize_sponsors(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if out.empty:
        return pd.DataFrame(
            columns=["matter_id", "person_id", "person_full_name", "sponsor_sequence", "sponsor_ward"]
        )

    out["matter_id"] = out[choose_column(out, ["matter_id", "MatterId"])].map(normalize_matter_id)

    person_col = choose_column(out, ["person_id", "PersonId", "MatterSponsorPersonId", "MatterSponsorId"])
    name_col = choose_column(out, ["person_full_name", "PersonFullName", "MatterSponsorName", "MatterSponsor"])
    seq_col = choose_column(out, ["sponsor_sequence", "MatterSponsorSequence", "MatterSponsorSort"])
    ward_col = choose_column(out, ["sponsor_ward", "MatterSponsorWard", "Ward", "person_ward"])

    out["person_id"] = out[person_col].map(clean_text) if person_col else None
    out["person_full_name"] = out[name_col].astype("string") if name_col else None
    out["sponsor_sequence"] = pd.to_numeric(out[seq_col], errors="coerce") if seq_col else None
    out["sponsor_ward"] = out[ward_col].astype("string") if ward_col else None

    keep = ["matter_id", "person_id", "person_full_name", "sponsor_sequence", "sponsor_ward"]
    return out[keep].drop_duplicates().reset_index(drop=True)


def normalize_histories(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if out.empty:
        return pd.DataFrame(columns=["matter_id", "action_date", "action_name", "action_body", "action_seq"])

    out["matter_id"] = out[choose_column(out, ["matter_id", "MatterId"])].map(normalize_matter_id)

    action_date_col = choose_column(out, ["action_date", "MatterHistoryActionDate", "ActionDate"])
    action_name_col = choose_column(out, ["action_name", "MatterHistoryActionName", "ActionName"])
    action_body_col = choose_column(
        out, ["action_body", "MatterHistoryActionBodyName", "ActionBodyName"]
    )
    seq_col = choose_column(out, ["action_seq", "MatterHistorySequence", "MatterHistoryId"])

    out["action_date"] = (
        pd.to_datetime(out[action_date_col], errors="coerce", utc=True).dt.tz_convert(None)
        if action_date_col
        else pd.NaT
    )
    out["action_name"] = out[action_name_col].astype("string") if action_name_col else None
    out["action_body"] = out[action_body_col].astype("string") if action_body_col else None
    out["action_seq"] = pd.to_numeric(out[seq_col], errors="coerce") if seq_col else None

    out = out.sort_values(["matter_id", "action_date", "action_seq"], na_position="last").reset_index(drop=True)
    if out["action_seq"].isna().all():
        out["action_seq"] = out.groupby("matter_id").cumcount() + 1

    keep = ["matter_id", "action_date", "action_name", "action_body", "action_seq"]
    return out[keep]


def normalize_attachments(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if out.empty:
        return pd.DataFrame(columns=["matter_id", "attachment_id", "attachment_name", "attachment_url"])

    out["matter_id"] = out[choose_column(out, ["matter_id", "MatterId"])].map(normalize_matter_id)

    aid_col = choose_column(out, ["attachment_id", "MatterAttachmentId", "AttachmentId", "Id"])
    aname_col = choose_column(out, ["attachment_name", "MatterAttachmentName", "AttachmentName", "Name"])
    aurl_col = choose_column(
        out,
        ["attachment_url", "MatterAttachmentHyperlink", "AttachmentHyperlink", "Hyperlink", "Url"],
    )

    out["attachment_id"] = out[aid_col].astype("string") if aid_col else None
    out["attachment_name"] = out[aname_col].astype("string") if aname_col else None
    out["attachment_url"] = out[aurl_col].astype("string") if aurl_col else None

    keep = ["matter_id", "attachment_id", "attachment_name", "attachment_url"]
    return out[keep].drop_duplicates().reset_index(drop=True)


def normalize_pdf_fields(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if out.empty:
        return pd.DataFrame(
            columns=[
                "matter_id",
                "attachment_id",
                "attachment_url",
                "local_pdf_path",
                "download_status",
                "parse_status",
                "file_hash",
                "text_chars",
                "from_zoning",
                "to_zoning",
                "from_zoning_raw",
                "to_zoning_raw",
                "from_zoning_canonical",
                "to_zoning_canonical",
                "zoning_parse_method",
                "zoning_parse_quality",
                "map_ref",
                "error_message",
            ]
        )

    out["matter_id"] = out[choose_column(out, ["matter_id", "MatterId"])].map(normalize_matter_id)
    out["attachment_id"] = out[choose_column(out, ["attachment_id", "MatterAttachmentId", "AttachmentId", "Id"])].astype(
        "string"
    )

    for col in [
        "attachment_url",
        "local_pdf_path",
        "download_status",
        "parse_status",
        "file_hash",
        "from_zoning",
        "to_zoning",
        "from_zoning_raw",
        "to_zoning_raw",
        "from_zoning_canonical",
        "to_zoning_canonical",
        "zoning_parse_method",
        "map_ref",
        "error_message",
    ]:
        src = choose_column(out, [col])
        if src:
            out[col] = out[src].astype("string")
        elif col not in out.columns:
            out[col] = None

    tchars_col = choose_column(out, ["text_chars"])
    out["text_chars"] = pd.to_numeric(out[tchars_col], errors="coerce") if tchars_col else None
    q_col = choose_column(out, ["zoning_parse_quality"])
    out["zoning_parse_quality"] = pd.to_numeric(out[q_col], errors="coerce") if q_col else None

    keep = [
        "matter_id",
        "attachment_id",
        "attachment_url",
        "local_pdf_path",
        "download_status",
        "parse_status",
        "file_hash",
        "text_chars",
        "from_zoning",
        "to_zoning",
        "from_zoning_raw",
        "to_zoning_raw",
        "from_zoning_canonical",
        "to_zoning_canonical",
        "zoning_parse_method",
        "zoning_parse_quality",
        "map_ref",
        "error_message",
    ]
    return out[keep].drop_duplicates().reset_index(drop=True)


def normalize_pdf_text(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    if out.empty:
        return pd.DataFrame(columns=["matter_id", "attachment_id", "page_number", "page_text"])

    out["matter_id"] = out[choose_column(out, ["matter_id", "MatterId"])].map(normalize_matter_id)
    out["attachment_id"] = out[
        choose_column(out, ["attachment_id", "MatterAttachmentId", "AttachmentId", "Id"])
    ].astype("string")
    page_col = choose_column(out, ["page_number", "PageNumber"])
    text_col = choose_column(out, ["page_text", "PageText"])
    out["page_number"] = pd.to_numeric(out[page_col], errors="coerce") if page_col else None
    out["page_text"] = out[text_col].astype("string") if text_col else None
    keep = ["matter_id", "attachment_id", "page_number", "page_text"]
    return out[keep].drop_duplicates().reset_index(drop=True)


def add_title_fields(matters: pd.DataFrame) -> pd.DataFrame:
    out = matters.copy()
    if out.empty:
        for col in ["map_grid", "address_raw", "app_number", "pd_number"]:
            if col not in out.columns:
                out[col] = pd.Series(dtype="string")
        return out

    parsed = out["matter_title"].apply(parse_title_fields).apply(pd.Series)
    out = pd.concat([out, parsed], axis=1)
    for col in ["map_grid", "address_raw", "app_number", "pd_number"]:
        if col not in out.columns:
            out[col] = pd.Series(dtype="string")
    return out


def add_duration_metrics(matters: pd.DataFrame, actions: pd.DataFrame) -> pd.DataFrame:
    out = matters.copy()

    referral = pd.DataFrame(columns=["matter_id", "first_zoning_referral_date"])
    if not actions.empty:
        mask = (
            actions["action_name"].fillna("").str.contains("referred", case=False)
            & actions["action_body"].fillna("").str.contains("zoning", case=False)
        )
        referral = (
            actions.loc[mask, ["matter_id", "action_date"]]
            .sort_values(["matter_id", "action_date"])
            .groupby("matter_id", as_index=False)
            .first()
            .rename(columns={"action_date": "first_zoning_referral_date"})
        )

    out = out.merge(referral, on="matter_id", how="left")
    out["matter_intro_date"] = pd.to_datetime(out["matter_intro_date"], errors="coerce", utc=True).dt.tz_convert(None)
    out["matter_passed_date"] = pd.to_datetime(out["matter_passed_date"], errors="coerce", utc=True).dt.tz_convert(None)
    out["first_zoning_referral_date"] = pd.to_datetime(
        out["first_zoning_referral_date"], errors="coerce", utc=True
    ).dt.tz_convert(None)
    out["days_intro_to_first_zoning_referral"] = (
        out["first_zoning_referral_date"] - out["matter_intro_date"]
    ).dt.days
    out["days_intro_to_passed"] = (out["matter_passed_date"] - out["matter_intro_date"]).dt.days
    return out


def build_primary_sponsor(sponsors: pd.DataFrame) -> pd.DataFrame:
    if sponsors.empty:
        return pd.DataFrame(columns=["matter_id", "primary_sponsor_name", "primary_sponsor_ward"])

    ranked = sponsors.copy()
    ranked["sponsor_sequence"] = pd.to_numeric(ranked["sponsor_sequence"], errors="coerce")
    ranked = ranked.sort_values(
        ["matter_id", "sponsor_sequence", "person_full_name"],
        na_position="last",
    )

    primary = ranked.groupby("matter_id", as_index=False).first()
    primary = primary.rename(
        columns={
            "person_full_name": "primary_sponsor_name",
            "sponsor_ward": "primary_sponsor_ward",
        }
    )
    return primary[["matter_id", "primary_sponsor_name", "primary_sponsor_ward"]]


def build_sponsors_output(sponsors: pd.DataFrame) -> pd.DataFrame:
    if sponsors.empty:
        return pd.DataFrame(
            columns=[
                "matter_id",
                "person_id",
                "person_full_name",
                "ward",
                "sponsor_sequence",
                "is_primary",
            ]
        )

    out = sponsors.copy()
    out = out.sort_values(["matter_id", "sponsor_sequence", "person_full_name"], na_position="last")
    out["is_primary"] = out.groupby("matter_id").cumcount().eq(0)
    out = out.rename(columns={"sponsor_ward": "ward"})
    keep = ["matter_id", "person_id", "person_full_name", "ward", "sponsor_sequence", "is_primary"]
    return out[keep].reset_index(drop=True)


def build_attachments_output(attachments: pd.DataFrame, pdf_fields: pd.DataFrame) -> pd.DataFrame:
    out = attachments.copy()
    if out.empty:
        return pd.DataFrame(
            columns=[
                "matter_id",
                "attachment_id",
                "attachment_name",
                "attachment_url",
                "local_pdf_path",
                "download_status",
                "parse_status",
                "file_hash",
                "text_chars",
            ]
        )

    meta = pdf_fields[
        [
            "matter_id",
            "attachment_id",
            "local_pdf_path",
            "download_status",
            "parse_status",
            "file_hash",
            "text_chars",
        ]
    ].drop_duplicates()

    out = out.merge(meta, on=["matter_id", "attachment_id"], how="left")
    keep = [
        "matter_id",
        "attachment_id",
        "attachment_name",
        "attachment_url",
        "local_pdf_path",
        "download_status",
        "parse_status",
        "file_hash",
        "text_chars",
    ]
    return out[keep].reset_index(drop=True)


def build_pdf_matter_fields(pdf_fields: pd.DataFrame) -> pd.DataFrame:
    if pdf_fields.empty:
        return pd.DataFrame(
            columns=[
                "matter_id",
                "from_zoning",
                "to_zoning",
                "from_zoning_raw",
                "to_zoning_raw",
                "from_zoning_canonical",
                "to_zoning_canonical",
                "zoning_parse_method",
                "zoning_parse_quality",
                "map_ref_pdf",
            ]
        )

    temp = pdf_fields.copy()
    temp["parse_rank"] = temp["parse_status"].eq("parsed").astype(int)
    if "zoning_parse_quality" in temp.columns:
        temp["zoning_parse_quality"] = pd.to_numeric(temp["zoning_parse_quality"], errors="coerce").fillna(0).astype(int)
    else:
        temp["zoning_parse_quality"] = 0
    temp["pair_quality"] = temp.apply(
        lambda row: parsed_pair_quality(row.get("from_zoning"), row.get("to_zoning")),
        axis=1,
    )
    temp = temp.sort_values(
        ["matter_id", "parse_rank", "zoning_parse_quality", "pair_quality", "text_chars"],
        ascending=[True, False, False, False, False],
    )

    agg = temp.groupby("matter_id", as_index=False).agg(
        {
            "from_zoning": first_non_null,
            "to_zoning": first_non_null,
            "from_zoning_raw": first_non_null,
            "to_zoning_raw": first_non_null,
            "from_zoning_canonical": first_non_null,
            "to_zoning_canonical": first_non_null,
            "zoning_parse_method": first_non_null,
            "zoning_parse_quality": "max",
            "map_ref": first_non_null,
        }
    )
    agg = agg.rename(columns={"map_ref": "map_ref_pdf"})
    return agg


def build_pdf_matter_text(pdf_text: pd.DataFrame) -> pd.DataFrame:
    if pdf_text.empty:
        return pd.DataFrame(columns=["matter_id", "attachment_id_text", "pdf_text", "pdf_text_chars"])

    temp = pdf_text.copy()
    temp["page_number"] = pd.to_numeric(temp["page_number"], errors="coerce")
    temp = temp.sort_values(["matter_id", "attachment_id", "page_number"], na_position="last")

    merged = temp.groupby(["matter_id", "attachment_id"], as_index=False).agg(
        pdf_text=("page_text", lambda s: "\n".join([x for x in s.fillna("") if x]))
    )
    merged["pdf_text"] = merged["pdf_text"].astype("string")
    merged["pdf_text_chars"] = merged["pdf_text"].fillna("").str.len()
    merged = merged.sort_values(["matter_id", "pdf_text_chars"], ascending=[True, False])
    best = merged.groupby("matter_id", as_index=False).first()
    best = best.rename(columns={"attachment_id": "attachment_id_text"})
    return best[["matter_id", "attachment_id_text", "pdf_text", "pdf_text_chars"]]


def reparse_from_best_text(
    matters: pd.DataFrame,
    text_col: str,
    source_tag: str,
) -> pd.DataFrame:
    out = matters.copy()
    if text_col not in out.columns:
        return out

    if "from_zoning_raw" not in out.columns:
        out["from_zoning_raw"] = None
    if "to_zoning_raw" not in out.columns:
        out["to_zoning_raw"] = None
    if "from_zoning_canonical" not in out.columns:
        out["from_zoning_canonical"] = None
    if "to_zoning_canonical" not in out.columns:
        out["to_zoning_canonical"] = None

    for idx, row in out.iterrows():
        current_quality = parsed_pair_quality(row.get("from_zoning"), row.get("to_zoning"))
        if current_quality >= 2:
            continue
        text = row.get(text_col)
        if pd.isna(text) or not str(text).strip():
            continue
        from_candidate, to_candidate = parse_from_to_zoning(text)
        candidate_quality = parsed_pair_quality(from_candidate, to_candidate)
        if candidate_quality <= current_quality:
            continue

        out.at[idx, "from_zoning"] = from_candidate
        out.at[idx, "to_zoning"] = to_candidate
        out.at[idx, "from_zoning_raw"] = from_candidate
        out.at[idx, "to_zoning_raw"] = to_candidate
        out.at[idx, "from_zoning_canonical"] = parse_zoning_code_for_quality(from_candidate)
        out.at[idx, "to_zoning_canonical"] = parse_zoning_code_for_quality(to_candidate)
        out.at[idx, "zoning_parse_method"] = f"reparse_{source_tag}"
        out.at[idx, "zoning_parse_quality"] = candidate_quality
    return out


def main() -> int:
    args = parse_args()
    ensure_parent(args.out_csv)

    matters_raw = pd.read_csv(args.in_matters, dtype=str, low_memory=False)
    sponsors_raw = pd.read_csv(args.in_sponsors, dtype=str, low_memory=False)
    histories_raw = pd.read_csv(args.in_histories, dtype=str, low_memory=False)
    attachments_raw = pd.read_csv(args.in_attachments, dtype=str, low_memory=False)
    pdf_fields_raw = pd.read_csv(args.in_pdf_fields, dtype=str, low_memory=False)
    pdf_text_raw = (
        pd.read_csv(args.in_pdf_text, dtype=str, low_memory=False)
        if args.in_pdf_text and Path(args.in_pdf_text).exists()
        else pd.DataFrame(columns=["matter_id", "attachment_id", "page_number", "page_text"])
    )
    candidates_raw = pd.read_csv(args.in_candidates, dtype=str, low_memory=False)
    councilmatic_fields_raw = pd.read_csv(args.in_councilmatic_fields, dtype=str, low_memory=False)

    candidates = normalize_candidates(candidates_raw)
    final_ids = set(
        candidates.loc[candidates["is_final_candidate"].fillna(False), "matter_id"]
        .dropna()
        .tolist()
    )

    matters = normalize_matters(matters_raw)
    sponsors = normalize_sponsors(sponsors_raw)
    histories = normalize_histories(histories_raw)
    attachments = normalize_attachments(attachments_raw)
    pdf_fields = normalize_pdf_fields(pdf_fields_raw)
    pdf_text = normalize_pdf_text(pdf_text_raw)

    if final_ids:
        matters = matters[matters["matter_id"].isin(final_ids)].copy()
        sponsors = sponsors[sponsors["matter_id"].isin(final_ids)].copy()
        histories = histories[histories["matter_id"].isin(final_ids)].copy()
        attachments = attachments[attachments["matter_id"].isin(final_ids)].copy()
        pdf_fields = pdf_fields[pdf_fields["matter_id"].isin(final_ids)].copy()
        pdf_text = pdf_text[pdf_text["matter_id"].isin(final_ids)].copy()
    else:
        matters = matters.iloc[0:0].copy()
        sponsors = sponsors.iloc[0:0].copy()
        histories = histories.iloc[0:0].copy()
        attachments = attachments.iloc[0:0].copy()
        pdf_fields = pdf_fields.iloc[0:0].copy()
        pdf_text = pdf_text.iloc[0:0].copy()

    actions_out = histories.copy()
    actions_out["action_date"] = pd.to_datetime(actions_out["action_date"], errors="coerce").dt.date.astype("string")

    sponsors_out = build_sponsors_output(sponsors)
    primary_sponsor = build_primary_sponsor(sponsors)
    attachments_out = build_attachments_output(attachments, pdf_fields)
    pdf_matter = build_pdf_matter_fields(pdf_fields)
    pdf_matter_text = build_pdf_matter_text(pdf_text)

    matters = add_title_fields(matters)
    matters = matters.merge(
        candidates[["matter_id", "zoning_class"]], on="matter_id", how="left"
    )
    matters = matters.merge(primary_sponsor, on="matter_id", how="left")
    matters = matters.merge(pdf_matter, on="matter_id", how="left")
    matters = matters.merge(pdf_matter_text, on="matter_id", how="left")
    for col in [
        "from_zoning",
        "to_zoning",
        "from_zoning_raw",
        "to_zoning_raw",
        "from_zoning_canonical",
        "to_zoning_canonical",
        "zoning_parse_method",
    ]:
        if col not in matters.columns:
            matters[col] = pd.Series([None] * len(matters), dtype="string")
        else:
            matters[col] = matters[col].astype("string")
    if "zoning_parse_quality" not in matters.columns:
        matters["zoning_parse_quality"] = 0
    matters["zoning_parse_quality"] = pd.to_numeric(matters["zoning_parse_quality"], errors="coerce").fillna(0).astype(int)
    matters["pdf_text"] = matters["pdf_text"].astype("string")

    # Repair weak/bad parse outputs using the best PDF text before detection flags.
    matters = reparse_from_best_text(matters, text_col="pdf_text", source_tag="pdf_text")

    matters["from_to_pdf_detected"] = matters["from_zoning"].notna() & matters["to_zoning"].notna()
    matters["ordinance_text"] = matters["pdf_text"]
    matters["ordinance_text_source"] = None
    matters.loc[matters["pdf_text"].notna(), "ordinance_text_source"] = "pdf"
    matters.loc[
        matters["ordinance_text_source"].isna()
        & (matters["from_zoning"].notna() | matters["to_zoning"].notna()),
        "ordinance_text_source",
    ] = "pdf"

    councilmatic_fields = normalize_councilmatic_fields(
        councilmatic_fields_raw, councilmatic_target_rows(matters)
    )
    councilmatic_fields = councilmatic_fields.drop(columns=["matter_file"])
    matters = matters.merge(councilmatic_fields, on="matter_id", how="left")
    matters["councilmatic_from_zoning"] = matters["councilmatic_from_zoning"].astype("string")
    matters["councilmatic_to_zoning"] = matters["councilmatic_to_zoning"].astype("string")
    matters["councilmatic_text"] = matters["councilmatic_text"].astype("string")
    matters["from_zoning"] = matters["from_zoning"].fillna(matters["councilmatic_from_zoning"])
    matters["to_zoning"] = matters["to_zoning"].fillna(matters["councilmatic_to_zoning"])
    matters["ordinance_text"] = matters["ordinance_text"].fillna(matters["councilmatic_text"])
    matters.loc[
        matters["ordinance_text_source"].isna()
        & matters["ordinance_text"].notna()
        & matters["councilmatic_status"].eq("ok"),
        "ordinance_text_source",
    ] = "councilmatic"
    matters.loc[
        matters["ordinance_text_source"].isna()
        & matters["ordinance_text"].notna()
        & matters["councilmatic_status"].isin(
            ["ok_pdf", "ok_pdf_ocr", "ok_pdf_backfill", "ok_pdf_backfill_ocr"]
        ),
        "ordinance_text_source",
    ] = "councilmatic_pdf"
    matters.loc[
        matters["ordinance_text_source"].isna() & matters["ordinance_text"].notna(),
        "ordinance_text_source",
    ] = "councilmatic"

    # Second-pass repair from best ordinance text after Councilmatic fallback.
    matters = reparse_from_best_text(matters, text_col="ordinance_text", source_tag="ordinance_text")

    matters["ordinance_text_chars"] = matters["ordinance_text"].fillna("").str.len()
    matters.loc[matters["ordinance_text_chars"].eq(0), "ordinance_text_chars"] = pd.NA

    matters = add_duration_metrics(matters, histories)
    matters["title_map_detected"] = matters["matter_title"].map(has_map_reclassification_title)
    matters["non_base_zoning_action"] = matters["ordinance_text"].map(is_non_base_zoning_action)
    matters.loc[matters["non_base_zoning_action"], "title_map_detected"] = False
    matters["rezoning_detection_method"] = None
    both_detected = matters["title_map_detected"] & matters["from_to_pdf_detected"]
    matters.loc[both_detected, "rezoning_detection_method"] = "both"
    matters.loc[
        matters["title_map_detected"] & ~matters["from_to_pdf_detected"],
        "rezoning_detection_method",
    ] = "title_map"
    matters.loc[
        ~matters["title_map_detected"] & matters["from_to_pdf_detected"],
        "rezoning_detection_method",
    ] = "pdf_from_to"
    matters = matters[matters["rezoning_detection_method"].notna()].copy()
    included_matter_ids = set(matters["matter_id"].dropna().tolist())
    actions_out = actions_out[actions_out["matter_id"].isin(included_matter_ids)].copy()
    sponsors_out = sponsors_out[sponsors_out["matter_id"].isin(included_matter_ids)].copy()
    attachments_out = attachments_out[attachments_out["matter_id"].isin(included_matter_ids)].copy()

    matters["matter_intro_date"] = pd.to_datetime(matters["matter_intro_date"], errors="coerce").dt.date.astype("string")
    matters["matter_passed_date"] = pd.to_datetime(matters["matter_passed_date"], errors="coerce").dt.date.astype("string")
    matters["first_zoning_referral_date"] = (
        pd.to_datetime(matters["first_zoning_referral_date"], errors="coerce").dt.date.astype("string")
    )

    matters_out = matters[
        [
            "matter_id",
            "matter_guid",
            "matter_file",
            "matter_title",
            "zoning_class",
            "source_system",
            "date_imputed_from_final_action",
            "rezoning_detection_method",
            "matter_status_name",
            "matter_body_name",
            "matter_intro_date",
            "matter_passed_date",
            "map_grid",
            "address_raw",
            "app_number",
            "pd_number",
            "map_ref_pdf",
            "from_zoning",
            "to_zoning",
            "from_zoning_raw",
            "to_zoning_raw",
            "from_zoning_canonical",
            "to_zoning_canonical",
            "zoning_parse_method",
            "zoning_parse_quality",
            "ordinance_text_chars",
            "ordinance_text_source",
            "councilmatic_url",
            "councilmatic_pdf_url",
            "councilmatic_status",
            "primary_sponsor_name",
            "primary_sponsor_ward",
            "first_zoning_referral_date",
            "days_intro_to_first_zoning_referral",
            "days_intro_to_passed",
        ]
    ].sort_values("matter_id")

    intro_dates = pd.to_datetime(matters_out["matter_intro_date"], errors="coerce")
    matters_out = matters_out.loc[
        intro_dates.between(args.sample_start_date, args.sample_end_date, inclusive="both")
    ].copy()

    matters_out.to_csv(args.out_csv, index=False)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
