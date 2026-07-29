import re

MAP_REF_RE = re.compile(r"Map No\.\s*([0-9]+-[A-Z])", re.IGNORECASE)
TO_THOSE_OF_PATTERN = (
    r"\b(?:to|lo)[\s'`\",.;:-]*(?:those|that|lho\.?se)[\s'`\",.;:-]*"
    r"(?:of|ot|0f|o[!l1])[\s'`\",.;:-]*(?:a|an)?\s*"
)
TO_THOSE_OF_RE = re.compile(TO_THOSE_OF_PATTERN, re.IGNORECASE)
ZONE_CODE_RE = re.compile(r"\b[A-Z]{1,4}\d?(?:\s*[- ]\s*|\s+)\d(?:\.\d+)?[A-Z]?\b", re.IGNORECASE)
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
ZONE_NOISE_PATTERNS = [
    re.compile(r"\bin order to\b.*$", re.IGNORECASE),
    re.compile(r"\bto allow\b.*$", re.IGNORECASE),
    re.compile(r"\bto permit\b.*$", re.IGNORECASE),
    re.compile(r"\bto develop\b.*$", re.IGNORECASE),
    re.compile(r"\bfor the purposes?\b.*$", re.IGNORECASE),
    re.compile(r"\bfor purposes?\b.*$", re.IGNORECASE),
    re.compile(r"\bsection\s*2\b.*$", re.IGNORECASE),
    re.compile(r"\bsection\s*3\b.*$", re.IGNORECASE),
    re.compile(r"\bthis ordinance\b.*$", re.IGNORECASE),
    re.compile(r"\bcommon address\b.*$", re.IGNORECASE),
    re.compile(r"\bsymbols?\s+and\s+indications?.*$", re.IGNORECASE),
]
ZONE_END_PATTERNS = [
    re.compile(r"\bsection\s*2\b", re.IGNORECASE),
    re.compile(r"\bthis ordinance\b", re.IGNORECASE),
    re.compile(r"\bcommon address\b", re.IGNORECASE),
]
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
        r"changing(?:\s+all)?(?:\s+of)?(?:\s+the)?\s+(?P<from>.+?)\s+symbols?"
        rf"(?:\s+and\s+indications?)?.*?{TO_THOSE_OF_PATTERN}"
        r"(?P<to>.+?)(?:\.\s*(?:section\s*2|this ordinance|common address)\b|\bsection\s*2\b|[.;])",
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
JUNK_ZONE_PATTERNS = [
    re.compile(r"\bowner allowing\b", re.IGNORECASE),
    re.compile(r"\bowner allowing the applicant\b", re.IGNORECASE),
    re.compile(r"\bapplicant\b", re.IGNORECASE),
    re.compile(r"\bproceed\b", re.IGNORECASE),
    re.compile(r"\bachieve full disclosure\b", re.IGNORECASE),
    re.compile(r"\btime\b", re.IGNORECASE),
]


def _clean(text: str | None) -> str | None:
    if text is None:
        return None
    value = re.sub(r"\s+", " ", str(text)).strip(" .;:\t\n\r")
    return value or None


def _normalize_numeric_token(value: str | None) -> str:
    if not value:
        return ""
    out = str(value).upper().strip()
    return out.replace("I", "1").replace("L", "1").replace("O", "0").replace("S", "5").replace("$", "5")


def _is_plausible_zoning_code(code: str | None) -> bool:
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


def preprocess_ordinance_text(text: str | None) -> str | None:
    if not text:
        return None
    out = str(text).replace("\u00a0", " ")
    out = re.sub(r"\bofthe\b", "of the", out, flags=re.IGNORECASE)
    out = re.sub(r"\ballthe\b", "all the", out, flags=re.IGNORECASE)
    out = re.sub(r"\btlie\b", "the", out, flags=re.IGNORECASE)
    out = out.replace("\u2018", "'").replace("\u2019", "'").replace("\u201c", '"').replace("\u201d", '"')
    out = re.sub(r"(?<=\w)[`'\",;:]+(?=\w)", " ", out)
    out = re.sub(r"\b33-(\d)\b", r"B3-\1", out, flags=re.IGNORECASE)
    out = re.sub(r"\b83-(\d)\b", r"B3-\1", out, flags=re.IGNORECASE)
    out = re.sub(r"\bBZ-(\d)\b", r"B2-\1", out, flags=re.IGNORECASE)
    out = re.sub(r"\bBZ\b(?=[\s\"'\[]+Neighborhood\s+Mixed-Use)", "B2-1", out, flags=re.IGNORECASE)
    out = re.sub(r"\bBZ\b", "B2", out, flags=re.IGNORECASE)
    out = re.sub(r"\b([A-Z]{1,4}\d)\.(\d)\b", r"\1-\2", out, flags=re.IGNORECASE)
    out = re.sub(r"\bRMS\.?5\b", "RM-5.5", out, flags=re.IGNORECASE)
    out = re.sub(r"\bRMS\b", "RM-5", out, flags=re.IGNORECASE)
    out = re.sub(r"\bRM-S\b", "RM-5", out, flags=re.IGNORECASE)
    out = re.sub(r"\bRSI\b", "RS-1", out, flags=re.IGNORECASE)
    out = re.sub(r"\bRTS\.5\b", "RT-3.5", out, flags=re.IGNORECASE)
    out = re.sub(r"\bto\s+(?:those|that)\s+ot\b", "to those of", out, flags=re.IGNORECASE)
    out = re.sub(r"\bto\s+(?:those|that)\s+0f\b", "to those of", out, flags=re.IGNORECASE)
    out = re.sub(r"\blo\s+(?:those|that)\s+(?:of|ot|0f|o[!l1])\b", "to those of", out, flags=re.IGNORECASE)
    out = re.sub(r"\bto\s+lho\.?se\s+(?:of|ot|0f|o[!l1])\b", "to those of", out, flags=re.IGNORECASE)
    out = re.sub(r"\bto\s+(?:those|that)\s+o[!l1]\b", "to those of", out, flags=re.IGNORECASE)
    out = re.sub(r"\s+", " ", out).strip()
    return out or None


def _extract_changing_symbols_pair(text: str) -> tuple[str | None, str | None]:
    start = re.search(r"\bchanging\b", text, re.IGNORECASE)
    if not start:
        return None, None

    zone_delimiter = re.search(
        r"\b(?:symbols?|syrnbols?|indications?)\b|(?=,?\s*(?:as\s+shown|shown)\s+on\s+map\b)",
        text[start.end() :],
        re.IGNORECASE,
    )
    if not zone_delimiter:
        return None, None

    from_raw = _clean(text[start.end() : start.end() + zone_delimiter.start()])
    if from_raw:
        from_raw = re.sub(r"^(?:all\s+)?(?:of\s+)?(?:the\s+)?", "", from_raw, flags=re.IGNORECASE).strip()
    tail = text[start.end() + zone_delimiter.end() :]
    to_matches = list(TO_THOSE_OF_RE.finditer(tail))
    if not to_matches:
        return from_raw, None

    to_tail = tail[to_matches[-1].end() :]
    cut_idx = len(to_tail)
    for pattern in ZONE_END_PATTERNS:
        match = pattern.search(to_tail)
        if match:
            cut_idx = min(cut_idx, match.start())
    to_raw = _clean(to_tail[:cut_idx])
    return from_raw, to_raw


def _looks_like_zone_text(value: str | None) -> bool:
    if not value:
        return False
    text = str(value)
    if canonicalize_zone_code(text):
        return True
    if re.search(r"\bplanned development\b", text, re.IGNORECASE):
        return True
    if re.search(r"\bopen space\b", text, re.IGNORECASE):
        return True
    if re.search(r"\btransportation district\b", text, re.IGNORECASE):
        return True
    return False


def canonicalize_zone_code(value: str | None) -> str | None:
    text = _clean(value)
    if not text:
        return None
    text = text.upper().replace("\u2013", "-").replace("\u2014", "-")
    text = re.sub(r"\s+", " ", text).strip()
    text = re.sub(r"\b33-(\d)\b", r"B3-\1", text)
    text = re.sub(r"\b83-(\d)\b", r"B3-\1", text)
    text = re.sub(r"\bBZ-(\d)\b", r"B2-\1", text)
    text = re.sub(r"\bBZ\b(?=[\s\"'\[]+NEIGHBORHOOD\s+MIXED-USE)", "B2-1", text)
    text = re.sub(r"\bBZ\b", "B2", text)
    text = re.sub(r"\b([A-Z]{1,4}\d)\.(\d)\b", r"\1-\2", text)
    text = text.replace("BL-I", "BL-1").replace("BI-I", "BI-1").replace("ML-I", "ML-1")
    text = text.replace("B1-I", "B1-1").replace("B2-I", "B2-1").replace("B3-I", "B3-1")
    text = text.replace("M1-I", "M1-1").replace("M2-I", "M2-1").replace("M3-I", "M3-1")
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
    text = re.sub(r"\bR\s*1\s*([34])[\.\-]?\s*5\b", r"RT-\1.5", text)
    text = re.sub(r"\b(RS|RT|RM|DR|DX|DC|DS)(\d(?:\.\d+)?)\b", r"\1-\2", text)
    text = re.sub(r"\b(RS|RT|RM)\s*-\s*(\d)\s*-\s*(\d)\b", r"\1-\2.\3", text)
    text = re.sub(r"\b([R][SMT])(\d)-(\d)\b", r"\1-\2.\3", text)
    text = re.sub(r"\b([BCM])-(\d)\b", r"\1\2", text)
    text = re.sub(r"\b([A-Z]{1,4}\d?)\s+(\d(?:\.\d+)?[A-Z]?)\b", r"\1-\2", text)
    text = re.sub(r"\b([A-Z]{1,4})\s*-\s*(\d)\s*-\s*(\d(?:\.\d+)?[A-Z]?)\b", r"\1\2-\3", text)
    text = re.sub(r"\s*-\s*", "-", text)
    text = text.replace("-L", "-1")
    if "PLANNED DEVELOPMENT" in text or re.search(r"\b(?:PD|RBPD|BPD|RPD|IPD)\b", text):
        return "PD"
    if re.search(r"\bPOS\b", text) or "OPEN SPACE" in text:
        return "POS"
    if text == "T" or text.startswith("T-") or text.startswith("T "):
        return "T"
    pmd = re.search(r"\bPMD\s*(?:#|NO\.?)?\s*([0-9ILSO]+)?\b", text)
    if pmd:
        num = _normalize_numeric_token(pmd.group(1))
        return f"PMD-{num}" if num else "PMD"
    match = re.search(r"\b([A-Z]{1,4}\d?-\d{1,2}(?:\.\d)?[A-Z]?)\b", text)
    if match:
        code = match.group(1)
        if _is_plausible_zoning_code(code):
            return code
    old_r = re.search(r"\bR([1-8])\b", text)
    if old_r:
        return f"R{old_r.group(1)}"
    c4 = re.search(r"\bC\s*[- ]?4\b", text)
    if c4:
        return "C4"
    return None


def normalize_zone_value(value: str | None) -> str | None:
    out = _clean(value)
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
    out = re.sub(r"^to those of (?:a|an)\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^that of (?:a|an)\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^of\s+the\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^current\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^its\s+current\s+", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^district\s+(?=[A-Z0-9])", "", out, flags=re.IGNORECASE).strip()
    out = re.sub(r"^(?:the|a|an)\s+", "", out, flags=re.IGNORECASE).strip()
    for pattern in ZONE_NOISE_PATTERNS:
        out = pattern.sub("", out).strip()
    out = re.sub(r"\band\s*$", "", out, flags=re.IGNORECASE).strip()
    out = out.strip(" .;:,")
    if not out:
        return None
    if any(pattern.search(out) for pattern in JUNK_ZONE_PATTERNS):
        return None
    if not _looks_like_zone_text(out):
        return None
    return out


def has_ordered_code_sequence_context(text: str | None) -> bool:
    candidate = _clean(text)
    if not candidate:
        return False
    return bool(
        re.search(
            r"\bchanging\b|\bcurrent\s+zoning\s+designation\b|\bzoning\s+change\s+from\b|\brezone\s+from\b|\brezoned\s+from\b",
            candidate,
            re.IGNORECASE,
        )
    )


def parse_from_to_zoning_detailed(text: str | None) -> dict:
    if not text:
        return {
            "from_zoning_raw": None,
            "to_zoning_raw": None,
            "from_zoning_clean": None,
            "to_zoning_clean": None,
            "from_zoning_canonical": None,
            "to_zoning_canonical": None,
            "parse_method": "none",
            "parse_quality": 0,
        }

    candidate = preprocess_ordinance_text(text)
    if not candidate:
        return {
            "from_zoning_raw": None,
            "to_zoning_raw": None,
            "from_zoning_clean": None,
            "to_zoning_clean": None,
            "from_zoning_canonical": None,
            "to_zoning_canonical": None,
            "parse_method": "none",
            "parse_quality": 0,
        }

    best = {
        "from_raw": None,
        "to_raw": None,
        "from_clean": None,
        "to_clean": None,
        "from_canonical": None,
        "to_canonical": None,
        "method": "none",
        "score": 0,
    }

    def consider_pair(raw_from: str | None, raw_to: str | None, method: str) -> None:
        from_clean = normalize_zone_value(raw_from)
        to_clean = normalize_zone_value(raw_to)
        from_canonical = canonicalize_zone_code(from_clean)
        to_canonical = canonicalize_zone_code(to_clean)
        if from_canonical and to_canonical:
            score = 2
        elif from_canonical or to_canonical:
            score = 1
        else:
            score = 0
        if score > best["score"]:
            best["from_raw"] = raw_from
            best["to_raw"] = raw_to
            best["from_clean"] = from_clean
            best["to_clean"] = to_clean
            best["from_canonical"] = from_canonical
            best["to_canonical"] = to_canonical
            best["method"] = method
            best["score"] = score

    first_from, first_to = _extract_changing_symbols_pair(candidate)
    if first_from or first_to:
        consider_pair(first_from, first_to, "changing_symbols")

    for pattern in FROM_TO_PATTERNS:
        for match in pattern.finditer(candidate):
            consider_pair(_clean(match.group("from")), _clean(match.group("to")), "from_to_pattern")
            if best["score"] >= 2:
                break
        if best["score"] >= 2:
            break

    from_raw = best["from_raw"]
    to_raw = best["to_raw"]
    from_clean = best["from_clean"]
    to_clean = best["to_clean"]
    from_canonical = best["from_canonical"]
    to_canonical = best["to_canonical"]
    method = best["method"]

    if not (from_canonical and to_canonical) and has_ordered_code_sequence_context(candidate):
        sequence_codes = extract_canonical_code_sequence(candidate)
        if sequence_codes:
            if from_canonical and not to_canonical:
                alt = next((code for code in sequence_codes if code != from_canonical), None)
                if alt:
                    to_canonical = alt
                    to_clean = to_clean or alt
            elif to_canonical and not from_canonical:
                alt = next((code for code in sequence_codes if code != to_canonical), None)
                if alt:
                    from_canonical = alt
                    from_clean = from_clean or alt
            elif len(sequence_codes) >= 2:
                from_canonical = sequence_codes[0]
                to_canonical = sequence_codes[1]
                from_clean = from_clean or from_canonical
                to_clean = to_clean or to_canonical
            if from_canonical or to_canonical:
                method = "code_sequence" if method == "none" else f"{method}_sequence"

    quality = 0
    if from_clean or to_clean or from_canonical or to_canonical:
        quality = 1
    if (from_clean and to_clean) or (from_canonical and to_canonical):
        quality = 2

    return {
        "from_zoning_raw": from_raw,
        "to_zoning_raw": to_raw,
        "from_zoning_clean": from_clean,
        "to_zoning_clean": to_clean,
        "from_zoning_canonical": from_canonical,
        "to_zoning_canonical": to_canonical,
        "parse_method": method,
        "parse_quality": quality,
    }


def parse_from_to_zoning(text: str | None) -> tuple[str | None, str | None]:
    detail = parse_from_to_zoning_detailed(text)
    return detail["from_zoning_clean"], detail["to_zoning_clean"]


def extract_canonical_code_sequence(text: str | None) -> list[str]:
    if not text:
        return []
    working = _clean(text)
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
        add(canonicalize_zone_code(part))

    for match in re.finditer(
        r"\b(?:PLANNED DEVELOPMENT|RBPD|BPD|RPD|IPD|POS)\b|\bT\b(?=\s*(?:,|-|\s)TRANSPORTATION|\s+DISTRICT\b)",
        working,
    ):
        add(canonicalize_zone_code(match.group(0)))

    for match in re.finditer(r"\b([A-Z]{1,4}\d?)\s*[- ]?\s*([0-9ILSO]{1,2}(?:\.[0-9ILSO])?)([A-Z]?)\b", working):
        token = f"{match.group(1)}-{match.group(2)}{match.group(3)}"
        add(canonicalize_zone_code(token))

    return found


def parse_map_ref(text: str | None) -> str | None:
    if not text:
        return None
    match = MAP_REF_RE.search(text)
    if not match:
        return None
    return _clean(match.group(1))
