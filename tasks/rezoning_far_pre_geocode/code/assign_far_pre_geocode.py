import argparse
import math
import re

import pandas as pd

STRUCTURAL_NA_CODES = {"PD", "POS"}
THREE_PART_ZONE_RE = re.compile(r"\b([A-Z]{1,3})\s*-\s*(\d)\s*-\s*(\d+(?:\.\d+)?[A-Z]?)\b")
ZONING_CODE_RE = re.compile(r"\b([A-Z]{1,4}\d?)\s*[- ]?\s*(\d+(?:\.\d+)?)([A-Z]?)\b")
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
CODE_TOKEN_RE = re.compile(
    r"\b(?:[A-Z]{1,4}\d?\s*-\s*\d+(?:\.\d+)?[A-Z]?|R[1-8]|PD|POS|PMD(?:-\d+[A-Z]?)?|PMO(?:-\d+[A-Z]?)?|T)\b"
)


def is_structural_na_code(code: str | None) -> bool:
    if not code:
        return False
    value = str(code).upper().strip()
    return value in STRUCTURAL_NA_CODES or value.startswith("PMD") or value.startswith("PMO")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("date_tag")
    return parser.parse_args()


def choose_column(columns: list[str], options: list[str], contains: list[str] | None = None) -> str | None:
    lower_to_original = {col.lower(): col for col in columns}
    for option in options:
        if option.lower() in lower_to_original:
            return lower_to_original[option.lower()]
    if contains:
        for col in columns:
            lowered = col.lower()
            if all(fragment in lowered for fragment in contains):
                return col
    return None


def safe_float(value) -> float | None:
    if pd.isna(value):
        return None
    try:
        out = float(str(value).strip())
    except Exception:  # noqa: BLE001
        return None
    if math.isnan(out) or math.isinf(out):
        return None
    return out


def parse_lookup_date(value) -> pd.Timestamp | None:
    ts = pd.to_datetime(value, errors="coerce")
    if pd.isna(ts):
        return None
    return ts


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


def format_lookup_date(value: pd.Timestamp | None) -> str | None:
    if value is None or pd.isna(value):
        return None
    return value.strftime("%Y-%m-%d")


def clean_zone_text(value) -> str | None:
    if pd.isna(value):
        return None
    text = str(value).upper().strip()
    if not text:
        return None
    text = text.replace("\u2013", "-").replace("\u2014", "-")
    text = text.replace("$", "S").replace("~", "-")
    text = re.sub(r"[\[\]\{\}\(\)\"]", " ", text)
    text = re.sub(r"\s+", " ", text)
    text = re.sub(r"^\s*[NA]\s+(?=[A-Z0-9])", "", text)
    text = text.strip(" ,.;")
    text = re.sub(r"-L\b", "-1", text)
    text = re.sub(r"-I\b", "-1", text)
    text = text.replace("BL-I", "BL-1").replace("BI-I", "BI-1").replace("ML-I", "ML-1")
    text = text.replace("M1-I", "M1-1").replace("M2-I", "M2-1").replace("M3-I", "M3-1")
    text = text.replace("B1-I", "B1-1").replace("B2-I", "B2-1").replace("B3-I", "B3-1")
    text = re.sub(r"\bBI-(\d)\b", r"B1-\1", text)
    text = re.sub(r"\bCI-(\d)\b", r"C1-\1", text)
    text = re.sub(r"\b0([1-5])-(\d(?:\.\d+)?)\b", r"C\1-\2", text)
    text = re.sub(r"\b33-(\d)\b", r"B3-\1", text)
    text = re.sub(r"\b83-(\d)\b", r"B3-\1", text)
    text = re.sub(r"\bBZ-(\d)\b", r"B2-\1", text)
    text = re.sub(r"\bBZ\b(?=\s+NEIGHBORHOOD\s+MIXED-USE)", "B2-1", text)
    text = re.sub(r"\bBZ\b", "B2", text)
    text = re.sub(r"\bRMS\.?5\b", "RM-5.5", text)
    text = re.sub(r"\bRMS\b", "RM-5", text)
    text = re.sub(r"\bRM-S\b", "RM-5", text)
    text = re.sub(r"\bRSI\b", "RS-1", text)
    text = re.sub(r"\bRTS\.5\b", "RT-3.5", text)
    text = re.sub(r"\b(RS|RT|RM|DR|DX|DC|DS)\s*([0-9]+(?:\.[0-9]+)?)\b", r"\1-\2", text)
    text = re.sub(r"\b(RS|RT|RM)\s*-\s*(\d)\s*-\s*(\d)\b", r"\1-\2.\3", text)
    text = text.replace("M1.-4", "M1-4")
    text = text.replace("B1.4", "B1-4")
    text = text.replace("B24", "B2-4")
    text = text.replace("C24", "C2-4")
    text = re.sub(r"\b([BCM])\s*-\s*(\d)\b", r"\1\2", text)
    text = re.sub(r"\b([BCM])\s+(\d)\b", r"\1\2", text)
    text = text.replace("-,", "-")
    text = text.replace(",-", "-")
    text = re.sub(r"\b([BCM][1-7])\?(\d(?:\.\d+)?)\b", r"\1-\2", text)
    text = re.sub(r"\bRT\s*([34])\s*[\.-]\s*S\b", r"RT-\1.5", text)
    text = re.sub(r"\bRT\s*([34])\s*-\s*5\b", r"RT-\1.5", text)
    text = re.sub(r"\bR\s*1([34])\s*-\s*5\b", r"RT-\1.5", text)
    text = re.sub(r"\bR\s*1\s*([34])[\.\-]?\s*5\b", r"RT-\1.5", text)
    text = re.sub(r"\bR\s*\.\s*T\s*-\s*\.?\s*([34])\b", r"RT-\1", text)
    text = re.sub(r"\bR\s*\.\s*S\s*-?\s*([1-8])\b", r"RS-\1", text)
    text = re.sub(r"\bRT\s*([34])\s*\.?\s*5\b", r"RT-\1.5", text)
    text = re.sub(r"\b([BCMR][1-9])-(\d(?:\.\d+)?)(?=[A-Z]{2,})", r"\1-\2 ", text)
    text = re.sub(r"\b(C)\s+[LI1]\s*-\s*(\d(?:\.\d+)?)\b", r"C1-\2", text)
    text = re.sub(r"\b(M[1-3])\s*[-,\.]\s*(\d)\b", r"\1-\2", text)
    text = text.replace("M?,-", "M2-")
    text = text.replace("M? -", "M2-")
    text = text.replace("ALLM2-1", "M2-1")
    text = text.replace("C2-2MOTOR", "C2-2 MOTOR")
    text = text.replace("B3-3COMMUNITY", "B3-3 COMMUNITY")
    text = text.replace("B1-1NEIGHBORHOOD", "B1-1 NEIGHBORHOOD")
    text = text.replace("C1-1NEIGHBORHOOD", "C1-1 NEIGHBORHOOD")
    text = text.replace("C1-2NEIGHBORHOOD", "C1-2 NEIGHBORHOOD")
    text = text.replace("C1-3NEIGHBORHOOD", "C1-3 NEIGHBORHOOD")
    text = text.replace("B3 L", "B3-1")
    text = text.replace("81-1", "B1-1")
    text = text.replace("B1-L", "B1-1")
    text = text.replace("B2-L", "B2-1")
    text = text.replace("B3-L", "B3-1")
    text = text.replace("C1-L", "C1-1")
    text = text.replace("RSS", "RS-3")
    text = text.replace("RT3.S", "RT-3.5")
    text = text.replace("RT 3 .5", "RT-3.5")
    text = text.replace("RT4-5", "RT-4.5")
    text = text.replace("RT-4-5", "RT-4.5")
    text = text.replace("RM4.5", "RM-4.5")
    text = text.replace("RS#", "RS-3")
    text = re.sub(r"\bR[- ]?M[- ]?4[- ]?5\b", "RM-4.5", text)
    text = re.sub(r"\bRA[- ]?4[- ]?5\b", "RM-4.5", text)
    text = re.sub(r"\bRHD[- ]?4[- ]?5\b", "RM-4.5", text)
    text = re.sub(r"\b([BCM][1-7]|C[1-5])-\!(?=\b|[^A-Z0-9])", r"\1-1", text)
    text = re.sub(r"\b([BCM][1-7]|C[1-5]|RS|RT|RM|DR|DX|DC|DS)-L(?=[A-Z])", r"\1-1 ", text)
    text = re.sub(r"\bBL-L(?=[A-Z])", "B1-1 ", text)
    text = re.sub(r"\bCL-L(?=[A-Z])", "C1-1 ", text)
    text = re.sub(r"\b([BCM][1-7])-S\b", r"\1-5", text)
    text = re.sub(r"\bC\]-([1-7])\b", r"C1-\1", text)
    text = re.sub(r"\bM\?\s*[-,]?\s*(\d)\b", r"M2-\1", text)
    text = re.sub(r"\b132-(\d)\b", r"B2-\1", text)
    text = re.sub(r"\bB([1-5])\s+NEIGHBORHOOD SHOPPING DISTRICT\b", r"B1-\1 NEIGHBORHOOD SHOPPING DISTRICT", text)
    text = re.sub(r"\bB([1-5])\s+COMMUNITY SHOPPING DISTRICT\b", r"B3-\1 COMMUNITY SHOPPING DISTRICT", text)
    text = re.sub(r"\bC([1-5])\s+NEIGHBORHOOD COMMERCIAL DISTRICT\b", r"C1-\1 NEIGHBORHOOD COMMERCIAL DISTRICT", text)
    text = re.sub(r"\b([BCMR][1-9])-(\d(?:\.\d+)?)(?=[A-Z]{2,})", r"\1-\2 ", text)
    text = text.replace("RPD", "PD")
    text = re.sub(r"\b8([1-7]-\d(?:\.\d+)?)\b", r"B\1", text)
    return text or None


def parse_zoning_code(value) -> str | None:
    text = clean_zone_text(value)
    if not text:
        return None

    if "PLANNED DEVELOPMENT" in text or re.search(r"\bPD\b", text):
        return "PD"
    if "PLANNED MANUFACTURING DISTRICT" in text or re.search(r"\bPMD\b", text):
        pmd_num = re.search(r"\bPMD\s*[- ]?(\d+[A-Z]?)\b", text)
        if not pmd_num:
            pmd_num = re.search(r"\b(?:NO\.?|NUMBER)\s*(\d+[A-Z]?)\b", text)
        if pmd_num:
            return f"PMD-{pmd_num.group(1)}"
        return "PMD"
    if "PLANNED OFFICE/MANUFACTURING" in text or re.search(r"\bPMO\b", text):
        pmo_num = re.search(r"\bPMO\s*[- ]?(\d+[A-Z]?)\b", text)
        if pmo_num:
            return f"PMO-{pmo_num.group(1)}"
        return "PMO"
    if (
        re.search(r"\bPOS\b", text)
        or re.search(r"\bPARKS?\s+AND\s+OPEN\s+SPACE\b", text)
        or re.search(r"\bOPEN\s+SPACE\s+DISTRICT\b", text)
    ):
        return "POS"
    if (
        text == "T"
        or text.startswith("T TRANSPORTATION")
        or text.startswith("T-TRANSPORTATION")
        or text.startswith("T, TRANSPORTATION")
        or "TRANSPORTATION DISTRICT" in text
    ):
        return "T"

    old_r_match = re.search(r"\bR([1-8])\b", text)
    if old_r_match:
        return f"R{old_r_match.group(1)}"

    if re.search(r"\bC\s*[- ]?4\b", text):
        return "C4"

    code = None
    three_part = THREE_PART_ZONE_RE.search(text)
    if three_part:
        code = f"{three_part.group(1)}{three_part.group(2)}-{three_part.group(3)}"
    else:
        match = ZONING_CODE_RE.search(text)
        if match:
            code = f"{match.group(1)}-{match.group(2)}{match.group(3)}"

    if code is None:
        direct_match = re.search(r"\b([A-Z]{1,4}\d?-\d+(?:\.\d+)?[A-Z]?)\b", text)
        if direct_match:
            code = direct_match.group(1)

    if code is None:
        return None

    code = code.replace("--", "-")
    code = re.sub(r"\s+", "", code)
    code = code.replace("-L", "-1")
    code = code.replace("-I", "-1")
    code = code.replace("MI-", "M1-")
    code = code.replace("RTS-5", "RT-3.5")
    code = code.replace("RT-3S", "RT-3.5")
    code = code.replace("RT-4-5", "RT-4.5")
    code = code.replace("R-3U", "RT-3.5")
    code = re.sub(r"^((?:B[1-7]|C[1-5]|M[1-3]|BL|CL|CI|ML)-\d+(?:\.\d+)?)[A-Z]$", r"\1", code)
    code = re.sub(r"^(RM|RT|RS)(\d)-5$", r"\1-\2.5", code)
    code = re.sub(r"^R-13$", "RT-3.5", code)
    code = re.sub(r"^R-20$", "RT-4", code)
    code = re.sub(r"^RT-3$", "RT-3.5", code)
    code = re.sub(r"^RT-4\.5$", "RM-4.5", code)
    if code == "RM-4":
        code = "RM-4.5"
    code = re.sub(r"^R-(\d)$", r"R\1", code)
    if code == "RS3":
        code = "RS-3"
    if code == "C-4":
        code = "C4"
    if not is_plausible_zoning_code(code):
        return None
    return code


def parse_zoning_code_for_side(value, side: str) -> str | None:
    if side not in {"from", "to"}:
        return parse_zoning_code(value)
    codes = extract_all_zoning_codes(value)
    if len(codes) >= 2:
        text = clean_zone_text(value) or ""
        if re.search(r"\bTO\b", text) or re.search(r"\bFROM\b", text):
            return codes[0] if side == "from" else codes[-1]
        return codes[0] if side == "from" else codes[-1]
    return parse_zoning_code(value)


def extract_all_zoning_codes(value) -> list[str]:
    text = clean_zone_text(value)
    if not text:
        return []
    out: list[str] = []
    seen = set()
    for match in CODE_TOKEN_RE.finditer(text):
        code = parse_zoning_code(match.group(0))
        if not code:
            continue
        if code in seen:
            continue
        seen.add(code)
        out.append(code)
    return out


def infer_missing_side_code_from_context(
    row: pd.Series,
    side: str,
    other_code: str | None,
) -> str | None:
    if side not in {"from", "to"}:
        return None
    opposite = "to" if side == "from" else "from"
    fields = [
        f"{side}_zoning_canonical",
        f"{side}_zoning_raw",
        f"{side}_zoning",
        f"{opposite}_zoning_canonical",
        f"{opposite}_zoning_raw",
        f"{opposite}_zoning",
        "matter_title",
    ]
    codes: list[str] = []
    seen = set()
    for field in fields:
        if field not in row.index:
            continue
        for code in extract_all_zoning_codes(row.get(field)):
            if code in seen:
                continue
            seen.add(code)
            codes.append(code)
    if not codes:
        return None

    ordered = list(reversed(codes)) if side == "to" else codes
    if other_code:
        for code in ordered:
            if code != other_code:
                return code
        return None

    if len(codes) < 2:
        return None
    return ordered[0]


def load_far_lookup(path: str) -> dict[str, list[dict]]:
    df = pd.read_csv(path, dtype=str, low_memory=False)
    columns = list(df.columns)
    code_col = choose_column(columns, ["zone_code", "zoning_code", "district", "district_type", "code"], contains=["zone"])
    if code_col is None:
        code_col = columns[0]
    far_col = choose_column(columns, ["floor_area_ratio", "max_far", "far", "maximum_far"], contains=["far"])
    if far_col is None:
        raise ValueError("Could not identify FAR column in lookup table.")
    start_col = choose_column(columns, ["effective_start_date", "start_date", "valid_from"], contains=["start", "date"])
    end_col = choose_column(columns, ["effective_end_date", "end_date", "valid_to"], contains=["end", "date"])
    version_col = choose_column(columns, ["zoning_code_version", "version", "lookup_version"], contains=["version"])

    lookup: dict[str, list[dict]] = {}
    for _, row in df.iterrows():
        code = parse_zoning_code(row.get(code_col))
        far = safe_float(row.get(far_col))
        if not code or far is None:
            continue
        record = {
            "far": far,
            "start": parse_lookup_date(row.get(start_col)) if start_col else None,
            "end": parse_lookup_date(row.get(end_col)) if end_col else None,
            "version": str(row.get(version_col)).strip() if version_col and pd.notna(row.get(version_col)) else None,
        }
        lookup.setdefault(code, []).append(record)

    # Safety extensions from project notes.
    for code, far in {"RT-4A": 1.2, "RM-4.5": 1.7, "T": 1.5}.items():
        lookup.setdefault(code, []).append(
            {
                "far": far,
                "start": pd.Timestamp("2004-11-01"),
                "end": None,
                "version": "post_2004_manual",
            }
        )

    for code, records in lookup.items():
        records.sort(
            key=lambda rec: (
                rec["start"] is None,
                rec["start"] if rec["start"] is not None else pd.Timestamp.max,
            )
        )
        lookup[code] = records
    return lookup


def choose_lookup_record(records: list[dict], intro_date_value) -> dict | None:
    if not records:
        return None
    intro = pd.to_datetime(intro_date_value, errors="coerce")
    if pd.isna(intro):
        return records[-1]

    applicable: list[dict] = []
    for rec in records:
        start = rec.get("start")
        end = rec.get("end")
        if start is not None and intro < start:
            continue
        if end is not None and intro > end:
            continue
        applicable.append(rec)

    if applicable:
        applicable.sort(
            key=lambda rec: rec.get("start") if rec.get("start") is not None else pd.Timestamp.min,
            reverse=True,
        )
        return applicable[0]

    older = [rec for rec in records if rec.get("start") is not None and rec.get("start") <= intro]
    if older:
        older.sort(key=lambda rec: rec["start"], reverse=True)
        return older[0]
    return records[0]


def resolve_code(
    row: pd.Series,
    side: str,
    lookup: dict[str, list[dict]],
) -> tuple[str | None, str | None, str]:
    candidates: list[tuple[str, object]] = []
    existing_code_col = f"{side}_code"
    canonical_col = f"{side}_zoning_canonical"
    raw_detail_col = f"{side}_zoning_raw"
    raw_col = f"{side}_zoning"
    journal_col = f"journal_{side}_code"

    if row.get("record_source") == "clerk_journal_first_pass" and journal_col in row.index:
        candidates.append((journal_col, row.get(journal_col)))
    if raw_detail_col in row.index:
        candidates.append((raw_detail_col, row.get(raw_detail_col)))
    if raw_col in row.index:
        candidates.append((raw_col, row.get(raw_col)))
    if canonical_col in row.index:
        candidates.append((canonical_col, row.get(canonical_col)))
    if existing_code_col in row.index:
        candidates.append((existing_code_col, row.get(existing_code_col)))

    parsed_candidates: list[tuple[str, str]] = []
    for source, value in candidates:
        code = parse_zoning_code_for_side(value, side)
        if code is None:
            continue
        parsed_candidates.append((source, code))
        if is_structural_na_code(code) or code in lookup:
            return code, source, "ok"

    if parsed_candidates:
        source, code = parsed_candidates[0]
        return code, source, "lookup_missing"
    return None, None, "unparseable"


def code_status_for_lookup(code: str | None, lookup: dict[str, list[dict]]) -> str:
    if code is None:
        return "unparseable"
    if is_structural_na_code(code) or code in lookup:
        return "ok"
    return "lookup_missing"


def resolve_far(
    code: str | None,
    intro_date_value,
    lookup: dict[str, list[dict]],
) -> tuple[float | None, str | None, str | None, str | None, str]:
    if code is None:
        return None, None, None, None, "missing_code"
    if is_structural_na_code(code):
        return None, None, None, None, "structural_na"
    records = lookup.get(code)
    if not records:
        inferred = infer_far_when_lookup_missing(code)
        if inferred is not None:
            return inferred, "inferred_from_code", None, None, "ok"
        return None, None, None, None, "lookup_missing"
    rec = choose_lookup_record(records, intro_date_value)
    if rec is None:
        inferred = infer_far_when_lookup_missing(code)
        if inferred is not None:
            return inferred, "inferred_from_code", None, None, "ok"
        return None, None, None, None, "lookup_missing"
    return rec["far"], rec.get("version"), format_lookup_date(rec.get("start")), format_lookup_date(rec.get("end")), "ok"


def infer_far_when_lookup_missing(code: str | None) -> float | None:
    if not code:
        return None
    value = str(code).upper().strip()
    # Downtown D districts generally use the numeric suffix as FAR.
    d_match = re.match(r"^(DX|DC|DS|DR)-(\d+(?:\.\d+)?)$", value)
    if d_match:
        return safe_float(d_match.group(2))
    return None


def distinct_side_fars(value, intro_date_value, lookup: dict[str, list[dict]]) -> tuple[int, bool]:
    codes = extract_all_zoning_codes(value)
    fars = [resolve_far(code, intro_date_value, lookup)[0] for code in codes]
    complete = bool(codes) and all(far is not None for far in fars)
    return len(set(fars)) if complete else 0, complete


def main() -> int:
    args = parse_args()
    master = pd.read_csv(
        f"../input/zoning_matters_{args.date_tag}.csv",
        dtype=str,
        low_memory=False,
    )
    lookup = load_far_lookup("../input/zoning_far_lookup_clean.csv")

    sample_decisions = pd.read_csv(
        "../input/rezoning_sample_decisions_20101101_20160831.csv",
        dtype=str,
    )
    if sample_decisions["matter_id"].duplicated().any():
        raise ValueError("Rezoning sample decisions contain duplicate matter_id rows")
    if not set(sample_decisions["action"]).issubset({"include", "exclude"}):
        raise ValueError("Rezoning sample decision has an invalid action")

    master = master.loc[
        ~master["matter_id"].isin(sample_decisions.loc[sample_decisions["action"].eq("exclude"), "matter_id"])
    ].copy()
    for row in sample_decisions.loc[sample_decisions["action"].eq("include")].itertuples(index=False):
        mask = master["matter_id"].eq(row.matter_id)
        if mask.sum() == 0:
            added = {column: pd.NA for column in master.columns}
            added.update(
                {
                    "matter_id": row.matter_id,
                    "matter_file": row.matter_id,
                    "matter_title": row.matter_title,
                    "source_system": "hand_adjudication",
                    "rezoning_detection_method": "ordinance_review",
                    "matter_status_name": "90-FINAL",
                    "matter_body_name": "City Council",
                    "matter_intro_date": row.matter_intro_date,
                    "matter_passed_date": row.matter_passed_date,
                    "from_zoning": row.from_zoning,
                    "to_zoning": row.to_zoning,
                    "from_zoning_raw": row.from_zoning,
                    "to_zoning_raw": row.to_zoning,
                }
            )
            master = pd.concat([master, pd.DataFrame([added])], ignore_index=True)
        elif mask.sum() == 1:
            master.loc[mask, "matter_status_name"] = "90-FINAL"
            master.loc[mask, "matter_passed_date"] = row.matter_passed_date
        else:
            raise ValueError(f"Sample inclusion is not unique: {row.matter_id}")

    journal_ids: set[str] = set()
    journal_fills = pd.read_csv(
        "../input/journal_zoning_code_fills_20101101_20160831.csv",
        dtype=str,
    )
    if journal_fills["matter_id"].duplicated().any():
        raise ValueError("Journal code fills contain duplicate matter_id rows")
    missing_ids = sorted(set(journal_fills["matter_id"]) - set(master["matter_id"]))
    if missing_ids:
        raise ValueError(f"Journal code fills absent from rezoning data: {missing_ids}")
    journal_ids = set(journal_fills["matter_id"])
    fills = journal_fills.set_index("matter_id")
    for matter_id, row in fills.iterrows():
        mask = master["matter_id"].eq(matter_id)
        master.loc[mask, ["from_zoning", "from_zoning_raw"]] = row["from_code_journal"]
        master.loc[mask, ["to_zoning", "to_zoning_raw"]] = row["to_code_journal"]

    out = master.copy()
    out["from_code_count"] = out["from_zoning_raw"].map(lambda value: len(extract_all_zoning_codes(value)))
    out["to_code_count"] = out["to_zoning_raw"].map(lambda value: len(extract_all_zoning_codes(value)))
    out["from_distinct_far_count"] = 0
    out["to_distinct_far_count"] = 0
    out["far_transition_status"] = "scalar_candidate"
    out.loc[
        out["from_code_count"].gt(1) | out["to_code_count"].gt(1),
        "far_transition_status",
    ] = "requires_section_review"
    out["from_code"] = None
    out["to_code"] = None
    out["from_code_source"] = None
    out["to_code_source"] = None
    out["from_code_status"] = None
    out["to_code_status"] = None

    out["from_far"] = None
    out["to_far"] = None
    out["from_far_version"] = None
    out["to_far_version"] = None
    out["from_far_effective_start"] = None
    out["from_far_effective_end"] = None
    out["to_far_effective_start"] = None
    out["to_far_effective_end"] = None
    out["from_far_status"] = None
    out["to_far_status"] = None
    out["far_change"] = None
    out["is_upzone"] = None
    out["far_pair_status"] = None
    out["far_pair_source"] = None

    for idx, row in out.iterrows():
        from_far_count, from_side_complete = distinct_side_fars(
            row.get("from_zoning_raw"), row.get("matter_intro_date"), lookup
        )
        to_far_count, to_side_complete = distinct_side_fars(
            row.get("to_zoning_raw"), row.get("matter_intro_date"), lookup
        )
        out.at[idx, "from_distinct_far_count"] = from_far_count
        out.at[idx, "to_distinct_far_count"] = to_far_count

        from_code, from_source, from_code_status = resolve_code(row, "from", lookup)
        to_code, to_source, to_code_status = resolve_code(row, "to", lookup)

        if from_code is None and to_code is not None:
            inferred = infer_missing_side_code_from_context(row, "from", to_code)
            if inferred is not None:
                from_code = inferred
                from_source = "from_context_infer"
                from_code_status = code_status_for_lookup(from_code, lookup)
        if to_code is None and from_code is not None:
            inferred = infer_missing_side_code_from_context(row, "to", from_code)
            if inferred is not None:
                to_code = inferred
                to_source = "to_context_infer"
                to_code_status = code_status_for_lookup(to_code, lookup)

        out.at[idx, "from_code"] = from_code
        out.at[idx, "to_code"] = to_code
        out.at[idx, "from_code_source"] = from_source
        out.at[idx, "to_code_source"] = to_source
        out.at[idx, "from_code_status"] = from_code_status
        out.at[idx, "to_code_status"] = to_code_status

        intro = row.get("matter_intro_date")
        from_far, from_version, from_start, from_end, from_far_status = resolve_far(from_code, intro, lookup)
        to_far, to_version, to_start, to_end, to_far_status = resolve_far(to_code, intro, lookup)

        out.at[idx, "from_far"] = from_far
        out.at[idx, "to_far"] = to_far
        out.at[idx, "from_far_version"] = from_version
        out.at[idx, "to_far_version"] = to_version
        out.at[idx, "from_far_effective_start"] = from_start
        out.at[idx, "from_far_effective_end"] = from_end
        out.at[idx, "to_far_effective_start"] = to_start
        out.at[idx, "to_far_effective_end"] = to_end
        out.at[idx, "from_far_status"] = from_far_status
        out.at[idx, "to_far_status"] = to_far_status

        if from_far is not None and to_far is not None:
            change = round(float(to_far) - float(from_far), 6)
            out.at[idx, "far_change"] = change
            out.at[idx, "is_upzone"] = bool(change > 0)
            out.at[idx, "far_pair_status"] = "resolved_both"
            out.at[idx, "far_pair_source"] = "zoning_code_lookup"
        elif from_far is None and to_far is None:
            out.at[idx, "far_pair_status"] = "missing_both"
        else:
            out.at[idx, "far_pair_status"] = "missing_one_side"

        multiple_codes = row.get("from_code_count", 0) > 1 or row.get("to_code_count", 0) > 1
        if multiple_codes and from_side_complete and to_side_complete and from_far_count == 1 and to_far_count == 1:
            out.at[idx, "far_transition_status"] = "scalar_equivalent_codes"

    for matter_id in journal_ids:
        mask = out["matter_id"].eq(matter_id)
        out.loc[mask, ["from_code_source", "to_code_source"]] = ["council_journal", "council_journal"]

    code_corrections = []
    pd_corrections = pd.read_csv(
        "../input/pd_transition_code_corrections_20101101_20160831.csv",
        dtype=str,
    )
    pd_corrections = pd_corrections.rename(columns={"corrected_to_code": "to_code"})
    pd_corrections["to_far"] = pd.NA
    code_corrections.append(pd_corrections[["matter_id", "to_code", "to_far"]])

    destination_corrections = pd.concat(
        [
            pd.read_csv("../input/destination_code_corrections_20101101_20160831.csv", dtype=str),
            pd.read_csv("../input/destination_code_corrections_20160901_20201231.csv", dtype=str),
        ],
        ignore_index=True,
    ).rename(columns={"corrected_to_code": "to_code", "corrected_to_far": "to_far"})
    code_corrections.append(destination_corrections[["matter_id", "to_code", "to_far"]])

    if code_corrections:
        code_corrections = pd.concat(code_corrections, ignore_index=True)
        if code_corrections["matter_id"].duplicated().any():
            raise ValueError("Ordinance code corrections contain duplicate matter_id rows")
        missing_ids = sorted(set(code_corrections["matter_id"]) - set(out["matter_id"]))
        if missing_ids:
            raise ValueError(f"Ordinance code corrections absent from rezoning data: {missing_ids}")
        for row in code_corrections.itertuples(index=False):
            idx = out.index[out["matter_id"].eq(row.matter_id)][0]
            to_far, to_version, to_start, to_end, to_status = resolve_far(
                row.to_code, out.at[idx, "matter_intro_date"], lookup
            )
            if pd.notna(row.to_far):
                to_far = float(row.to_far)
                to_version = "ordinance_code_correction"
                to_start = None
                to_end = None
                to_status = "ok"
            out.at[idx, "to_code"] = row.to_code
            out.at[idx, "to_code_source"] = "ordinance_code_correction"
            out.at[idx, "to_code_status"] = code_status_for_lookup(row.to_code, lookup)
            out.at[idx, "to_far"] = to_far
            out.at[idx, "to_far_version"] = to_version
            out.at[idx, "to_far_effective_start"] = to_start
            out.at[idx, "to_far_effective_end"] = to_end
            out.at[idx, "to_far_status"] = to_status
            out.at[idx, "far_pair_source"] = "ordinance_code_correction"

    decisions = pd.concat(
        [
            pd.read_csv("../input/pd_to_pd_far_decisions_20101101_20160831.csv", dtype=str),
            pd.read_csv("../input/pd_to_pd_far_decisions_20160901_20201231.csv", dtype=str),
        ],
        ignore_index=True,
    )
    if decisions["matter_id"].duplicated().any():
        raise ValueError("PD-to-PD FAR decisions contain duplicate matter_id rows")
    if not decisions["confidence"].eq("high").all():
        raise ValueError("Only high-confidence PD-to-PD decisions may enter production")
    missing_ids = sorted(set(decisions["matter_id"]) - set(out["matter_id"]))
    if missing_ids:
        raise ValueError(f"PD-to-PD FAR decisions absent from rezoning data: {missing_ids}")

    for row in decisions.itertuples(index=False):
        idx = out.index[out["matter_id"].eq(row.matter_id)][0]
        old_far = float(row.old_far)
        new_far = float(row.new_far)
        change = round(new_far - old_far, 6)
        out.loc[idx, ["from_code", "to_code"]] = ["PD", "PD"]
        out.loc[idx, ["from_code_source", "to_code_source"]] = ["ordinance_pd_review", "ordinance_pd_review"]
        out.loc[idx, ["from_code_status", "to_code_status"]] = ["structural_na", "structural_na"]
        out.loc[idx, ["from_far", "to_far"]] = [old_far, new_far]
        out.loc[idx, ["from_far_version", "to_far_version"]] = ["ordinance_pd_review", "ordinance_pd_review"]
        out.loc[
            idx,
            [
                "from_far_effective_start",
                "from_far_effective_end",
                "to_far_effective_start",
                "to_far_effective_end",
            ],
        ] = [None, None, None, None]
        out.loc[idx, ["from_far_status", "to_far_status"]] = ["ok", "ok"]
        out.loc[idx, "far_change"] = change
        out.loc[idx, "is_upzone"] = bool(change > 0)
        out.loc[idx, "far_pair_status"] = "resolved_both"
        out.loc[idx, "far_pair_source"] = "ordinance_pd_review"
        out.loc[idx, "far_transition_status"] = "scalar_ordinance_review"

    section_decisions = pd.read_csv(
        "../input/section_review_decisions_20101101_20201231.csv",
        dtype=str,
    )
    if section_decisions["matter_id"].duplicated().any():
        raise ValueError("Section-review decisions contain duplicate matter_id rows")
    valid_section_decisions = {
        "non_scalar_multiple_far",
        "scalar_ordinance_review",
        "unresolved_destination_scope",
    }
    if not set(section_decisions["decision"]).issubset(valid_section_decisions):
        raise ValueError("Section-review decision has an invalid action")
    missing_ids = sorted(set(section_decisions["matter_id"]) - set(out["matter_id"]))
    if missing_ids:
        raise ValueError(f"Section-review decisions absent from rezoning data: {missing_ids}")

    scalar_section_decisions = section_decisions.loc[
        section_decisions["decision"].eq("scalar_ordinance_review")
    ]
    for row in scalar_section_decisions.itertuples(index=False):
        idx = out.index[out["matter_id"].eq(row.matter_id)][0]
        from_far, from_version, from_start, from_end, from_status = resolve_far(
            row.corrected_from_code, out.at[idx, "matter_intro_date"], lookup
        )
        to_far, to_version, to_start, to_end, to_status = resolve_far(
            row.corrected_to_code, out.at[idx, "matter_intro_date"], lookup
        )
        if from_far is None or to_far is None:
            raise ValueError(f"Section-review scalar decision lacks FAR values: {row.matter_id}")
        out.loc[idx, ["from_code", "to_code"]] = [row.corrected_from_code, row.corrected_to_code]
        out.loc[idx, ["from_code_source", "to_code_source"]] = [
            "section_ordinance_review",
            "section_ordinance_review",
        ]
        out.loc[idx, ["from_code_status", "to_code_status"]] = ["ok", "ok"]
        out.loc[idx, ["from_far", "to_far"]] = [from_far, to_far]
        out.loc[idx, ["from_far_version", "to_far_version"]] = [from_version, to_version]
        out.loc[idx, ["from_far_effective_start", "from_far_effective_end"]] = [from_start, from_end]
        out.loc[idx, ["to_far_effective_start", "to_far_effective_end"]] = [to_start, to_end]
        out.loc[idx, ["from_far_status", "to_far_status"]] = [from_status, to_status]
        out.loc[idx, "far_pair_source"] = "section_ordinance_review"
        out.loc[idx, "far_transition_status"] = "scalar_ordinance_review"

    non_scalar_decisions = pd.read_csv(
        "../input/non_scalar_far_decisions_20101101_20201231.csv",
        dtype=str,
    )
    if non_scalar_decisions["matter_id"].duplicated().any():
        raise ValueError("Non-scalar FAR decisions contain duplicate matter_id rows")
    missing_ids = sorted(set(non_scalar_decisions["matter_id"]) - set(out["matter_id"]))
    if missing_ids:
        raise ValueError(f"Non-scalar FAR decisions absent from rezoning data: {missing_ids}")

    automatic_non_scalar = out["far_transition_status"].eq("requires_section_review") & (
        out["from_distinct_far_count"].gt(1) | out["to_distinct_far_count"].gt(1)
    )
    reviewed_non_scalar = section_decisions.loc[
        section_decisions["decision"].eq("non_scalar_multiple_far"), "matter_id"
    ]
    non_scalar = (
        automatic_non_scalar
        | out["matter_id"].isin(non_scalar_decisions["matter_id"])
        | out["matter_id"].isin(reviewed_non_scalar)
    )
    unresolved_scope = out["matter_id"].isin(
        section_decisions.loc[
            section_decisions["decision"].eq("unresolved_destination_scope"), "matter_id"
        ]
    )

    out.loc[non_scalar, "far_transition_status"] = "non_scalar_multiple_far"
    out.loc[non_scalar, ["from_far", "to_far", "far_change", "is_upzone"]] = None
    out.loc[non_scalar, ["from_far_status", "to_far_status"]] = "non_scalar_multiple_far"
    out.loc[non_scalar, "far_pair_source"] = "non_scalar_review"
    out.loc[unresolved_scope, "far_transition_status"] = "unresolved_destination_scope"
    out.loc[unresolved_scope, ["from_far", "to_far", "far_change", "is_upzone"]] = None
    out.loc[unresolved_scope, ["from_far_status", "to_far_status"]] = "unresolved_destination_scope"
    out.loc[unresolved_scope, "far_pair_source"] = "section_ordinance_review"

    from_far = pd.to_numeric(out["from_far"], errors="coerce")
    to_far = pd.to_numeric(out["to_far"], errors="coerce")
    paired = from_far.notna() & to_far.notna() & ~non_scalar & ~unresolved_scope
    out.loc[paired, "far_change"] = (to_far[paired] - from_far[paired]).round(6)
    out.loc[paired, "is_upzone"] = out.loc[paired, "far_change"].astype(float).gt(0)
    out.loc[paired, "far_pair_status"] = "resolved_both"
    ordinary = ~non_scalar & ~unresolved_scope
    out.loc[ordinary & ~paired & from_far.isna() & to_far.isna(), "far_pair_status"] = "missing_both"
    out.loc[ordinary & ~paired & (from_far.notna() | to_far.notna()), "far_pair_status"] = "missing_one_side"
    out.loc[non_scalar, "far_pair_status"] = "non_scalar_multiple_far"
    out.loc[unresolved_scope, "far_pair_status"] = "unresolved_destination_scope"

    out.to_csv(f"../output/zoning_matters_far_{args.date_tag}.csv", index=False)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
