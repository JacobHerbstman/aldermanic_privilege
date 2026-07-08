import re
from typing import Iterable


ZONE_TOKEN = r"[A-Za-z0-9,\-\/\. ]{2,160}?(?:Districts?|Planned Development(?:\s+No\.?\s*\d+)?)"
MAP_LABEL = r"Map\s*(?:No\.?|\s*Number)"
MAP_ID = r"[0-9OISL](?:\s*[0-9OISL])?\s*-\s*[0-9A-Z/]"
INDEX_TO_LINK = r"(?:to|t0)"
INDEX_ZONE_BASE = (
    r"(?:"
    r"[A-Z]{1,4}[0-9ILS](?:\.[0-9])?(?:-[A-Z0-9\.]+)?"
    r"|DX-?\d+|DC-?\d+|POS-?\d+|PMD-?\d+|BPD|RBPD"
    r"|(?:BUS(?:INESS)?\.?\s*)?P(?:L|I)ND\.?\s*DEVEL\.?\s*NO\.?\s*\d+"
    r"|(?:RESIDENTIAL\s+)?P(?:L|I)ND\.?\s*DEVEL\.?\s*NO\.?\s*\d+"
    r"|PLANNED\s+DEVELOPMENT(?:\s+NO\.?\s*\d+)?"
    r"|PD\s*NO\.?\s*\d+"
    r")"
)
INDEX_ZONE_TOKEN = rf"{INDEX_ZONE_BASE}(?:\s+(?:AND|&)\s+{INDEX_ZONE_BASE})?"

COMMITTEE_RECLASS_RE = re.compile(
    rf"(?:(?P<applicant>[A-Z][A-Za-z0-9&\.,'()/\- ]{{2,140}}?)\s*[—-]\s*)?"
    rf"to\s+classify\s+as\s+(?P<to>{ZONE_TOKEN})\s+instead\s+of\s+(?P<from>{ZONE_TOKEN})\s+"
    rf"the\s+area\s+shown\s+on\s+{MAP_LABEL}\s*(?P<map>{MAP_ID})",
    flags=re.IGNORECASE,
)

ORDINANCE_RECLASS_RE = re.compile(
    rf"changing\s+all\s+the\s+(?P<from>{ZONE_TOKEN})\s+symbols(?:\s+and\s+indications)?\s+"
    rf"as\s+shown\s+on\s+{MAP_LABEL}\s*(?P<map>{MAP_ID}).*?"
    rf"to\s+those\s+of\s+(?:an?\s+)?(?P<to>{ZONE_TOKEN})",
    flags=re.IGNORECASE | re.DOTALL,
)

ORDINANCE_RECLASS_OCR_RE = re.compile(
    rf"changing\s+(?:all|a[ul]|edl)\s*(?:of\s*the|ofthe|the)?\s*(?P<from>{ZONE_TOKEN}).{{0,220}}?"
    rf"as\s+shown\s+on\s+{MAP_LABEL}\s*(?P<map>{MAP_ID}).*?"
    rf"to\s+those\s+of\s+(?:an?\s+)?(?P<to>{ZONE_TOKEN})",
    flags=re.IGNORECASE | re.DOTALL,
)

HEADING_MAP_RE = re.compile(
    rf"Reclassification\s+of\s+Area\s+Shown\s+on\s+{MAP_LABEL}\s*(?P<map>{MAP_ID})",
    flags=re.IGNORECASE,
)

MAP_REF_RE = re.compile(rf"{MAP_LABEL}\s*(?P<map>{MAP_ID})", flags=re.IGNORECASE)
ZONING_KEYWORD_RE = re.compile(
    r"\b(zoning|reclassification|planned\s+development|map\s+amendment)\b",
    flags=re.IGNORECASE,
)
INSTEAD_OF_RE = re.compile(
    rf"instead\s+of\s+(?:an?\s+)?(?P<from>{ZONE_TOKEN})",
    flags=re.IGNORECASE,
)
TO_THOSE_OF_RE = re.compile(
    rf"to\s*those\s+of\s+(?:an?\s+)?(?P<to>{ZONE_TOKEN})",
    flags=re.IGNORECASE,
)
TO_CLASSIFY_AS_RE = re.compile(
    rf"to\s+classify\s+as\s+(?P<to>{ZONE_TOKEN})",
    flags=re.IGNORECASE,
)
FROM_THAT_OF_RE = re.compile(
    rf"from\s+(?:that|those)\s+of\s+(?:an?\s+)?(?P<from>{ZONE_TOKEN})",
    flags=re.IGNORECASE,
)
INDEX_FROM_TO_PAREN_RE = re.compile(
    rf"\((?P<from>{INDEX_ZONE_TOKEN})\s*{INDEX_TO_LINK}\s*(?P<to>{INDEX_ZONE_TOKEN})\)",
    flags=re.IGNORECASE,
)
INDEX_FROM_TO_RE = re.compile(
    rf"(?P<from>{INDEX_ZONE_TOKEN})\s*{INDEX_TO_LINK}\s*(?P<to>{INDEX_ZONE_TOKEN})",
    flags=re.IGNORECASE,
)


def normalize_text(value: str | None) -> str:
    if not value:
        return ""
    out = str(value)
    out = out.replace("—", "-").replace("–", "-")
    out = out.replace("Oistrict", "District")
    out = out.replace("Oistricts", "Districts")
    out = out.replace("Districl", "District")
    out = out.replace("Districis", "Districts")
    out = re.sub(r"\b([A-Z])\s*l\s*-\s*([0-9])\b", r"\g<1>1-\g<2>", out)
    out = re.sub(r"\s+", " ", out)
    return out.strip()


def clean_value(value: str | None) -> str | None:
    if not value:
        return None
    out = normalize_text(value).strip(" ,;-")
    return out or None


def clean_map_no(value: str | None) -> str | None:
    cleaned = clean_value(value)
    if not cleaned:
        return None
    cleaned = cleaned.upper().replace(" ", "")
    cleaned = cleaned.replace("/", "1")
    cleaned = cleaned.strip(".,;:")
    cleaned = re.sub(r"(?<=\d)O|O(?=\d)", "0", cleaned)
    cleaned = re.sub(r"(?<=\d)[IL]|[IL](?=\d)", "1", cleaned)
    cleaned = re.sub(r"(?<=\d)S|S(?=\d)|^S(?=-)|(?<=-)S$", "5", cleaned)
    if re.fullmatch(r"[OSILB]", cleaned):
        cleaned = cleaned.translate(str.maketrans({"O": "0", "S": "5", "I": "1", "L": "1", "B": "8"}))
    return cleaned


def normalize_index_segment(value: str) -> str:
    out = normalize_text(value)
    out = re.sub(
        r"(?i)\b([A-Z0-9][A-Z0-9\.\-]{1,16})(?:t0|to)([A-Z][A-Z0-9\.\-]{0,16})\b",
        r"\1 to \2",
        out,
    )
    return out


def looks_like_index_page(text: str, map_hits: list[re.Match[str]]) -> bool:
    lowered = text.lower()
    return (
        len(map_hits) >= 3
        or "legislative index" in lowered
        or "zoning reclassifications" in lowered
        or "city clerk referred" in lowered
        or "zoning referred" in lowered
    )


def looks_like_zone_phrase(value: str | None) -> bool:
    if not value:
        return False
    lowered = value.lower()
    if len(lowered) > 100:
        return False
    return (
        bool(re.search(r"\b(?:[bcmr]\d|rs[0-9il]|rt[0-9il]|rm[0-9il]|dx-?\d|dc-?\d|pos-?\d|pmd-?\d|bpd|rbpd)\b", lowered))
        or "district" in lowered
        or "planned development" in lowered
        or "plnd" in lowered
        or "pind" in lowered
        or "pd no" in lowered
    )


def snippet_around(text: str, start: int, end: int, radius: int = 180) -> str:
    left = max(0, start - radius)
    right = min(len(text), end + radius)
    return normalize_text(text[left:right])


def extract_page_records(page_text: str, page_number: int) -> list[dict]:
    text = normalize_text(page_text)
    if not text:
        return []

    rows: list[dict] = []

    for match in COMMITTEE_RECLASS_RE.finditer(text):
        rows.append(
            {
                "page_number": page_number,
                "match_type": "committee_reclass",
                "map_no": clean_map_no(match.group("map")),
                "from_district_raw": clean_value(match.group("from")),
                "to_district_raw": clean_value(match.group("to")),
                "applicant_raw": clean_value(match.group("applicant")),
                "has_from_to": True,
                "snippet": snippet_around(text, match.start(), match.end(), radius=420),
            }
        )

    for ordinance_re in (ORDINANCE_RECLASS_RE, ORDINANCE_RECLASS_OCR_RE):
        for match in ordinance_re.finditer(text):
            rows.append(
                {
                    "page_number": page_number,
                    "match_type": "ordinance_reclass",
                    "map_no": clean_map_no(match.group("map")),
                    "from_district_raw": clean_value(match.group("from")),
                    "to_district_raw": clean_value(match.group("to")),
                    "applicant_raw": None,
                    "has_from_to": True,
                    "snippet": snippet_around(text, match.start(), match.end(), radius=420),
                }
            )

    heading_matches = list(HEADING_MAP_RE.finditer(text))
    maps_with_from_to = {row.get("map_no") for row in rows if row.get("has_from_to") and row.get("map_no")}

    for i, match in enumerate(heading_matches):
        map_no = clean_map_no(match.group("map"))
        block_end = heading_matches[i + 1].start() if i + 1 < len(heading_matches) else len(text)
        block_text = text[match.start() : block_end]

        from_match = FROM_THAT_OF_RE.search(block_text) or INSTEAD_OF_RE.search(block_text)
        to_match = TO_THOSE_OF_RE.search(block_text) or TO_CLASSIFY_AS_RE.search(block_text)
        inferred_from = clean_value(from_match.group("from")) if from_match else None
        inferred_to = clean_value(to_match.group("to")) if to_match else None

        if map_no and inferred_from and inferred_to:
            rows.append(
                {
                    "page_number": page_number,
                    "match_type": "heading_context_reclass",
                    "map_no": map_no,
                    "from_district_raw": inferred_from,
                    "to_district_raw": inferred_to,
                    "applicant_raw": None,
                    "has_from_to": True,
                    "snippet": snippet_around(text, match.start(), min(block_end, match.start() + 520)),
                }
            )
            maps_with_from_to.add(map_no)
            continue

        if map_no and map_no in maps_with_from_to:
            continue

        rows.append(
            {
                "page_number": page_number,
                "match_type": "heading_reclass",
                "map_no": map_no,
                "from_district_raw": None,
                "to_district_raw": None,
                "applicant_raw": None,
                "has_from_to": False,
                "snippet": snippet_around(text, match.start(), match.end()),
            }
        )

    seen_from_to = {
        (row.get("map_no"), row.get("from_district_raw"), row.get("to_district_raw"))
        for row in rows
        if row.get("has_from_to")
    }
    for map_match in MAP_REF_RE.finditer(text):
        map_no = clean_map_no(map_match.group("map"))
        if map_no is None:
            continue
        window = snippet_around(text, map_match.start(), map_match.end(), radius=520)
        if ZONING_KEYWORD_RE.search(window) is None:
            continue

        from_match = INSTEAD_OF_RE.search(window) or FROM_THAT_OF_RE.search(window)
        to_match = TO_THOSE_OF_RE.search(window) or TO_CLASSIFY_AS_RE.search(window)
        if from_match is None or to_match is None:
            continue

        from_value = clean_value(from_match.group("from"))
        to_value = clean_value(to_match.group("to"))
        if not from_value or not to_value:
            continue

        key = (map_no, from_value, to_value)
        if key in seen_from_to:
            continue
        seen_from_to.add(key)

        rows.append(
            {
                "page_number": page_number,
                "match_type": "heuristic_reclass",
                "map_no": map_no,
                "from_district_raw": from_value,
                "to_district_raw": to_value,
                "applicant_raw": None,
                "has_from_to": True,
                "snippet": window,
            }
        )

    map_hits = list(MAP_REF_RE.finditer(text))
    if map_hits and looks_like_index_page(text, map_hits):
        for i, map_hit in enumerate(map_hits):
            map_no = clean_map_no(map_hit.group("map"))
            if not map_no:
                continue
            seg_start = map_hit.end()
            seg_end = map_hits[i + 1].start() if i + 1 < len(map_hits) else min(len(text), seg_start + 420)
            segment = normalize_index_segment(text[seg_start:seg_end])
            if not segment:
                continue

            for from_to_re in (INDEX_FROM_TO_PAREN_RE, INDEX_FROM_TO_RE):
                for match in from_to_re.finditer(segment):
                    from_value = clean_value(match.group("from"))
                    to_value = clean_value(match.group("to"))
                    if not looks_like_zone_phrase(from_value) or not looks_like_zone_phrase(to_value):
                        continue
                    if from_value == to_value:
                        continue
                    rows.append(
                        {
                            "page_number": page_number,
                            "match_type": "index_reclass",
                            "map_no": map_no,
                            "from_district_raw": from_value,
                            "to_district_raw": to_value,
                            "applicant_raw": None,
                            "has_from_to": True,
                            "snippet": snippet_around(segment, match.start(), match.end(), radius=120),
                        }
                    )

    reliable_types = {"committee_reclass", "ordinance_reclass", "heading_context_reclass", "index_reclass"}
    reliable_maps_with_from_to = {
        row.get("map_no")
        for row in rows
        if row.get("has_from_to") and row.get("match_type") in reliable_types and row.get("map_no")
    }

    filtered_rows: list[dict] = []
    for row in rows:
        if row.get("match_type") == "heuristic_reclass":
            map_no = row.get("map_no")
            from_raw = clean_value(row.get("from_district_raw"))
            to_raw = clean_value(row.get("to_district_raw"))
            if map_no in reliable_maps_with_from_to:
                continue
            if from_raw and to_raw and from_raw.lower() == to_raw.lower():
                continue
        filtered_rows.append(row)
    rows = filtered_rows

    seen: set[tuple] = set()
    deduped: list[dict] = []
    for row in rows:
        key = (
            row.get("match_type"),
            row.get("map_no"),
            row.get("from_district_raw"),
            row.get("to_district_raw"),
            row.get("applicant_raw"),
        )
        if key in seen:
            continue
        seen.add(key)
        deduped.append(row)
    return deduped


def find_map_references(page_text: str) -> list[str]:
    text = normalize_text(page_text)
    if not text or ZONING_KEYWORD_RE.search(text) is None:
        return []
    refs = [clean_map_no(match.group("map")) for match in MAP_REF_RE.finditer(text)]
    out = [ref for ref in refs if ref]
    return sorted(set(out))


def dedupe_records(rows: Iterable[dict]) -> list[dict]:
    seen: set[tuple] = set()
    deduped: list[dict] = []
    for row in rows:
        key = (
            row.get("journal_filename"),
            row.get("page_number"),
            row.get("match_type"),
            row.get("map_no"),
            row.get("from_district_raw"),
            row.get("to_district_raw"),
            row.get("applicant_raw"),
        )
        if key in seen:
            continue
        seen.add(key)
        deduped.append(row)
    return deduped
