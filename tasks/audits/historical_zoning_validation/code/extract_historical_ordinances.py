# setwd("tasks/audits/historical_zoning_validation/code")

import json
import re
import subprocess
from pathlib import Path

import pandas as pd


MAP_LABEL = r"(?:Nu\w*(?:s)?|No(?:s)?\.?|N\s*O\s*S?\s*\.?)"
MAP_TOKEN = r"[0-9IL]{1,2}\s*-\s*[A-Z0-9]"
MAP_LIST = rf"{MAP_TOKEN}(?:\s*(?:,|&|A\s*N\s*D)\s*{MAP_TOKEN})*"
HEADER_END = r"(?:\s*[.:]\s*|\s*\n\s*)"

HEADER_RE = re.compile(
    r"Reclass\w*\s+(?:As\s+Amended\s+)?Of\s+Area(?:s)?\s+"
    r"S\s*h\s*o\s*w\s*n\s+On\s+Map\s+"
    rf"{MAP_LABEL}\s*"
    rf"(?P<maps>{MAP_LIST}){HEADER_END}"
    r"(?:\(\s*As\s+Amended\s*\)\s*)?"
    r"\(\s*App\w*\s+(?:Number|No\.?)\s+(?P<application>[A-Z0-9\- ]+)\s*\)",
    flags=re.IGNORECASE | re.DOTALL,
)

HEADER_RE_REORDERED = re.compile(
    r"Reclass\w*\s+(?:As\s+Amended\s+)?Of\s+Area(?:s)?\s+"
    r"S\s*h\s*o\s*w\s*n\s+On\s+Map\s+"
    rf"{MAP_LABEL}\s*"
    r"(?:\(\s*As\s+Amended\s*\)\s*)?"
    r"\(\s*App\w*\s+(?:Number|No\.?)\s+(?P<application>[A-Z0-9\- ]+)\s*\)\s*"
    rf"(?:\d{{4,6}}\s*)?(?P<maps>{MAP_LIST}){HEADER_END}",
    flags=re.IGNORECASE | re.DOTALL,
)

HEADER_RE_SPACED = re.compile(
    r"R\s*E\s*C\s*L\s*A\s*S\s*S\s*I\s*F\s*I\s*C\s*A\s*T\s*I\s*O\s*N"
    r".{0,180}?"
    r"A\s*R\s*E\s*A\s*S?\s*H\s*O\s*W\s*N"
    r".{0,80}?M\s*A\s*P\s*"
    rf"{MAP_LABEL}\s*"
    rf"(?P<maps>{MAP_LIST}){HEADER_END}"
    r"(?:\(\s*As\s+Amended\s*\)\s*)?"
    r"\(\s*App\w*\s+(?:Number|No\.?)\s+(?P<application>[A-Z0-9\- ]+)\s*\)",
    flags=re.IGNORECASE | re.DOTALL,
)

SECTION_RE = re.compile(
    r"^\s*S\s*E\s*C\s*T\s*I\s*O\s*N\s+[0-9IVXL]+\s*[,.:]",
    flags=re.IGNORECASE | re.MULTILINE,
)


def normalize_text(value: str) -> str:
    value = value.replace("\u2013", "-").replace("\u2014", "-")
    value = re.sub(r"\bRESIDEN(?:FIAL|RTTIAL)\b", "RESIDENTIAL", value, flags=re.IGNORECASE)
    value = re.sub(r"\bPLA(?:IMED|RTRTED)\b", "PLANNED", value, flags=re.IGNORECASE)
    value = re.sub(r"\bRMS\b", "RM5", value, flags=re.IGNORECASE)
    value = re.sub(r"\bRR(?=\s*-?\s*[1-8])", "RT", value, flags=re.IGNORECASE)
    return re.sub(r"\s+", " ", value).strip()


def normalize_application(value: str) -> str:
    return re.sub(r"[^A-Z0-9]", "", value.upper())


def zoning_groups(value: str) -> list[str]:
    text = normalize_text(value).upper()
    text = re.sub(r"\b8([1-7])\s*-", r"B\1-", text)
    text = re.sub(r"\b([BCM])\s*[LI]\s*-", r"\g<1>1-", text)

    patterns = [
        ("Planned Manufacturing", r"PLANNED MANUFACTURING|\bPMD\b"),
        ("Open Space", r"PARKS? AND OPEN SPACE|OPEN SPACE DISTRICT|\bPOS\b"),
        ("Planned Development", r"PLANNED DEVELOPMENT|\bPD\b"),
        (
            "Single-Family Residential",
            r"\bRS\s*-?\s*[1-8](?:\b|\.)|SINGLE-UNIT|DETACHED HOUSE",
        ),
        (
            "Multi-Family Residential",
            r"\b(?:RT|RM)\s*-?\s*[1-8](?:\b|\.)|TWO-FLAT|MULTI-UNIT",
        ),
        (
            "Neighborhood Mixed-Use",
            r"\bB[1-7]\s*-?\s*\d|NEIGHBORHOOD (?:MIXED-USE|SHOPPING)",
        ),
        ("Commercial", r"\bC[1-5]\s*-?\s*\d|COMMERCIAL DISTRICT"),
        (
            "Industrial",
            r"\bM[1-3]\s*-?\s*\d|MANUFACTURING(?:/BUSINESS PARK)? DISTRICT",
        ),
        (
            "Downtown",
            r"\b(?:DX|DR|DS|DC)\s*-?\s*\d|DOWNTOWN .*? DISTRICT",
        ),
        ("Other", r"TRANSPORTATION DISTRICT|\bT DISTRICT\b"),
    ]
    return [label for label, pattern in patterns if re.search(pattern, text)]


def parse_clause_transitions(block: str) -> list[dict]:
    starts = list(SECTION_RE.finditer(block))
    sections = []
    for index, match in enumerate(starts):
        end = starts[index + 1].start() if index + 1 < len(starts) else len(block)
        sections.append(normalize_text(block[match.end() : end]))
    if not sections:
        sections = [normalize_text(block)]

    transitions = []
    for section in sections:
        changing = re.search(
            r"\b(?:CHANG[A-Z]{1,12}|CHEMG[A-Z]{1,12}|REESTABLISH[A-Z]{1,12})\b",
            section,
            flags=re.IGNORECASE,
        )
        if changing is None:
            continue
        destination_after_change = re.search(
            r"\bTO\s+(?:THOSE\s+OF|THAT\s+OF|THE\s+DESIGNATION\s+OF)",
            section[changing.end() :],
            flags=re.IGNORECASE,
        )
        if destination_after_change is None:
            continue
        destination_start = changing.end() + destination_after_change.start()
        destination_end_position = changing.end() + destination_after_change.end()

        source_text = section[changing.end() : destination_start]
        source_end = re.search(
            r"\bSYMBOLS?\b|\bINDICATIONS?\b|\bAS\s+SHOWN\b|\bWITHIN\s+THE\s+AREA\b",
            source_text,
            flags=re.IGNORECASE,
        )
        if source_end is not None:
            source_text = source_text[: source_end.start()]

        destination_text = section[destination_end_position : destination_end_position + 500]
        destination_end = re.search(
            r"\bAND\s+A\s+CORRESPONDING\b|\bWHICH\s+IS\s+HEREBY\b|"
            r"\bAS\s+AMENDED\b|\bSUBJECT\s+TO\b|\bSECTION\b|[.;]",
            destination_text,
            flags=re.IGNORECASE,
        )
        if destination_end is not None:
            destination_text = destination_text[: destination_end.start()]
        if re.search(r"\bAND\s+THEN\s+TO\b", destination_text, flags=re.IGNORECASE):
            destination_text = re.split(
                r"\bAND\s+THEN\s+TO\b", destination_text, flags=re.IGNORECASE
            )[-1]

        source_groups = zoning_groups(source_text)
        destination_groups = zoning_groups(destination_text)
        if source_groups and len(destination_groups) == 1:
            transitions.append(
                {
                    "source_groups": source_groups,
                    "destination_group": destination_groups[0],
                }
            )

    unique = []
    seen = set()
    for transition in transitions:
        key = (tuple(transition["source_groups"]), transition["destination_group"])
        if key not in seen:
            seen.add(key)
            unique.append(transition)
    return unique


def extract_common_addresses(block: str) -> list[str]:
    matches = re.findall(
        r"COMMON\s+ADDRESS(?:\s+OF\s+PROPERTY)?\s*:?\s*(.*?)"
        r"(?:\)|\n|SECTION\s+[0-9IVXL]+)",
        block,
        flags=re.IGNORECASE | re.DOTALL,
    )
    addresses = []
    for match in matches:
        address = normalize_text(match).strip(" ,.;")
        if address and address.upper() not in {item.upper() for item in addresses}:
            addresses.append(address)
    return addresses


def extract_bounded_areas(block: str) -> list[str]:
    matches = re.findall(
        r"(?:AREA|PROPERTY)\s+BOUNDED\s+BY\s*:?\s*(.*?)"
        r"\bTO\s+(?:THOSE\s+OF|THAT\s+OF|THE\s+DESIGNATION\s+OF)",
        block,
        flags=re.IGNORECASE | re.DOTALL,
    )
    return [normalize_text(match).strip(" ,.;") for match in matches if normalize_text(match)]


manifest = pd.read_csv("../input/journal_manifest_2006_2012.csv", low_memory=False)
manifest = manifest.loc[
    manifest["year"].between(2006, 2012)
    & manifest["downloaded"].fillna(False).astype(bool)
    & manifest["is_valid_pdf"].fillna(False).astype(bool)
].copy()

records = []
for row in manifest.itertuples(index=False):
    pdf_path = Path("../input/journals_2006_2012") / row.rel_local_path
    result = subprocess.run(
        ["pdftotext", "-enc", "UTF-8", str(pdf_path), "-"],
        check=True,
        capture_output=True,
        text=True,
    )
    document_text = result.stdout
    header_candidates = list(HEADER_RE.finditer(document_text))
    header_candidates.extend(HEADER_RE_REORDERED.finditer(document_text))
    header_candidates.extend(HEADER_RE_SPACED.finditer(document_text))
    headers = []
    seen_headers = set()
    for header in sorted(header_candidates, key=lambda match: match.start()):
        key = (header.start(), normalize_application(header.group("application")))
        if key not in seen_headers:
            seen_headers.add(key)
            headers.append(header)

    for index, header in enumerate(headers):
        block_end = headers[index + 1].start() if index + 1 < len(headers) else len(document_text)
        block = document_text[header.start() : block_end]
        transitions = parse_clause_transitions(block)
        source_groups = sorted(
            {group for transition in transitions for group in transition["source_groups"]}
        )
        destination_groups = sorted(
            {transition["destination_group"] for transition in transitions}
        )
        common_addresses = extract_common_addresses(block)
        bounded_areas = extract_bounded_areas(block)

        records.append(
            {
                "journal_year": int(row.year),
                "journal_meeting_date": row.meeting_date,
                "journal_filename": row.filename,
                "journal_pdf_url": row.pdf_url,
                "pdf_page_number": document_text.count("\f", 0, header.start()) + 1,
                "application_number_raw": normalize_text(header.group("application")),
                "application_key": normalize_application(header.group("application")),
                "map_numbers_raw": normalize_text(header.group("maps")),
                "source_groups": ";".join(source_groups),
                "destination_groups": ";".join(destination_groups),
                "clause_count": len(transitions),
                "clause_transitions_json": json.dumps(transitions, separators=(",", ":")),
                "common_addresses": "; ".join(common_addresses),
                "bounded_area_count": len(bounded_areas),
                "bounded_areas": " | ".join(bounded_areas),
                "ordinance_excerpt": normalize_text(block[:8000]),
            }
        )

    print(f"Parsed {row.filename}: {len(headers)} ordinance blocks", flush=True)

ordinances = pd.DataFrame.from_records(records)
if ordinances.empty:
    raise RuntimeError("No historical zoning ordinance blocks were extracted.")

ordinances["date_application_block_count"] = ordinances.groupby(
    ["journal_meeting_date", "application_key"]
)["application_key"].transform("size")

ordinances = ordinances.sort_values(
    ["journal_meeting_date", "application_key", "pdf_page_number"], kind="stable"
)
ordinances.to_csv("../output/historical_zoning_ordinances_2006_2012.csv", index=False)
