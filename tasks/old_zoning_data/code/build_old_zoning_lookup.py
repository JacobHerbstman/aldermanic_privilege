import argparse
import re
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-old-zoning-txt", default="../input/chicago_zoning_code1957.txt")
    parser.add_argument("--in-crosswalk-csv", default="../input/zoning_conversion_2004_crosswalk.csv")
    parser.add_argument("--out-bulk-csv", default="../output/old_zoning_bulk_density_1957_2004.csv")
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def build_target_universe() -> list[str]:
    out: list[str] = []
    out.extend([f"R{i}" for i in range(1, 9)])

    for prefix in ["B1", "B2", "B3", "B4", "B5"]:
        out.extend([f"{prefix}-{i}" for i in range(1, 6)])
    out.extend(["B6-6", "B6-7", "B7-5", "B7-6", "B7-7"])

    for prefix in ["C1", "C2"]:
        out.extend([f"{prefix}-{i}" for i in range(1, 6)])
    out.extend([f"C3-{i}" for i in range(1, 8)])
    out.append("C4")
    out.extend([f"C5-{i}" for i in range(1, 6)])

    for prefix in ["M1", "M2", "M3"]:
        out.extend([f"{prefix}-{i}" for i in range(1, 6)])

    return out


def normalize_code(value: str | None) -> str | None:
    if value is None:
        return None
    out = str(value).upper().strip()
    if not out:
        return None

    out = out.replace("—", "-").replace("–", "-")
    out = out.replace(" ", "")

    # Common OCR artifacts in this source.
    out = out.replace("B1.4", "B1-4")
    out = out.replace("M1.-4", "M1-4")
    out = out.replace("B24", "B2-4")
    out = out.replace("C24", "C2-4")
    out = out.replace("C02", "C2")

    out = re.sub(r"^([BCM]\d)\.(\d)$", r"\1-\2", out)
    out = re.sub(r"^([BCM]\d)-\.(\d)$", r"\1-\2", out)
    out = re.sub(r"^([BCM]\d)(\d)$", r"\1-\2", out)
    out = re.sub(r"^R-(\d)$", r"R\1", out)

    if re.fullmatch(r"R[1-8]", out):
        return out
    if out == "C4":
        return out
    if re.fullmatch(r"[BCM][1-7]-\d(?:\.\d+)?", out):
        return out
    if re.fullmatch(r"[BCM][1-7]", out):
        return out
    return out


def district_family(code: str) -> str:
    if code.startswith("R"):
        return "residential"
    if code.startswith("B"):
        return "business"
    if code.startswith("C"):
        return "commercial"
    if code.startswith("M"):
        return "manufacturing"
    return "unknown"


def trim_to_ordinance_text(raw_text: str) -> str:
    article12_positions = [m.start() for m in re.finditer(r"ARTICLE\s*12\b", raw_text, flags=re.IGNORECASE)]
    if len(article12_positions) >= 2:
        return raw_text[: article12_positions[1]]
    return raw_text


def normalize_ocr_text(raw_text: str) -> str:
    text = raw_text.replace("—", "-").replace("–", "-")
    text = text.replace("În", "In")
    text = text.replace("Ân", "In")
    text = text.replace("B1.4", "B1-4")
    text = text.replace("M1.-4", "M1-4")
    text = text.replace("B24", "B2-4")
    text = text.replace("C24", "C2-4")
    return text


def collapse_text_for_regex(raw_text: str) -> str:
    text = normalize_ocr_text(raw_text)
    text = re.sub(r"(\w)-\s*\n\s*(\w)", r"\1\2", text)
    text = re.sub(r"\s+", " ", text)
    return text


def parse_mentions(lines: list[str], target_set: set[str]) -> pd.DataFrame:
    token_re = re.compile(r"\b(R[1-8]|C4|[BCM][1-7](?:[\.-]\d(?:\.\d+)?)?|[BCM][1-7])\b")
    section_re = re.compile(r"^\s*(\d+\.\d+(?:-\d+)?)\b")

    rows: list[dict] = []
    current_section = None

    for i, line in enumerate(lines, start=1):
        sec = section_re.search(line)
        if sec:
            current_section = sec.group(1)

        for match in token_re.finditer(line):
            raw_token = match.group(1)
            code = normalize_code(raw_token)
            rows.append(
                {
                    "line_number": i,
                    "raw_token": raw_token,
                    "district_code": code,
                    "section_hint": current_section,
                    "line_text": line.strip(),
                }
            )

    mentions = pd.DataFrame(rows)
    if mentions.empty:
        mentions = pd.DataFrame(columns=["line_number", "raw_token", "district_code", "section_hint", "line_text"])

    if not mentions.empty:
        mentions = mentions[mentions["district_code"].notna()].copy()

    grouped = (
        mentions.groupby("district_code", dropna=False)
        .agg(
            mention_count=("district_code", "size"),
            first_line_number=("line_number", "min"),
            last_line_number=("line_number", "max"),
            section_hint=("section_hint", "first"),
            sample_line=("line_text", "first"),
        )
        .reset_index()
    ) if not mentions.empty else pd.DataFrame(columns=[
        "district_code",
        "mention_count",
        "first_line_number",
        "last_line_number",
        "section_hint",
        "sample_line",
    ])

    target_df = pd.DataFrame({"district_code": sorted(target_set)})
    full = target_df.merge(grouped, on="district_code", how="left")
    full["is_target_universe"] = True
    full["mentioned_in_text"] = full["mention_count"].fillna(0).astype(int) > 0

    extras = grouped[~grouped["district_code"].isin(target_set)].copy()
    if not extras.empty:
        extras["is_target_universe"] = False
        extras["mentioned_in_text"] = True
        full = pd.concat([full, extras], ignore_index=True, sort=False)

    for col in ["mention_count", "first_line_number", "last_line_number"]:
        full[col] = pd.to_numeric(full[col], errors="coerce").astype("Int64")

    return full.sort_values(["is_target_universe", "district_code"], ascending=[False, True], kind="stable")


def infer_far_section(code: str) -> str:
    if code.startswith("R"):
        return "7.6"
    if code.startswith("B"):
        return "8.5"
    if code.startswith("C"):
        return "9.5"
    if code.startswith("M"):
        return "10.12"
    return ""


def extract_far_metrics(collapsed_text: str) -> dict[str, dict]:
    far_re = re.compile(
        r"In\s+(?:an?|the)\s+([A-Z0-9\.-]+)\s+District,\s+the\s+floor\s+area\w*\s+ratio\s+shall\s+not\s+ex(?:ceed|ced)\s+([0-9]+(?:\.[0-9]+)?)",
        flags=re.IGNORECASE,
    )

    far_matches = list(far_re.finditer(collapsed_text))
    out: dict[str, dict] = {}

    for idx, match in enumerate(far_matches):
        code = normalize_code(match.group(1))
        if not code:
            continue

        basic_far = float(match.group(2))
        end = far_matches[idx + 1].start() if idx + 1 < len(far_matches) else min(len(collapsed_text), match.start() + 1600)
        window = collapsed_text[match.start() : end]

        max_far = None
        max_far_note = None

        explicit_increase = re.search(r"may\s+be\s+increased\s+to\s+([0-9]+(?:\.[0-9]+)?)", window, flags=re.IGNORECASE)
        has_15pct = bool(re.search(r"may\s+be\s+increased\s+by\s+15\s+per\s+cent", window, flags=re.IGNORECASE))
        has_premium = bool(re.search(r"floor\s+area\s+ratio\s+premiums?\s+may\s+be\s+added", window, flags=re.IGNORECASE))

        if explicit_increase:
            explicit_val = float(explicit_increase.group(1))
            if explicit_val > basic_far:
                max_far = explicit_val
                max_far_note = "Explicit increased FAR value in district text"
            else:
                max_far = None
                max_far_note = "Explicit increase phrase found but value was not above basic FAR; ignored as OCR/context artifact"
        elif has_15pct and not has_premium:
            max_far = round(basic_far * 1.15, 4)
            max_far_note = "15 percent FAR increase noted in district text"
        elif has_15pct and has_premium:
            max_far = None
            max_far_note = "15 percent FAR increase plus additional premium formulas"
        elif has_premium:
            max_far = None
            max_far_note = "Additional FAR premium formulas apply"

        out[code] = {
            "basic_far": basic_far,
            "max_far": max_far,
            "max_far_note": max_far_note,
            "source_method": "direct_text",
            "source_section": infer_far_section(code),
            "source_excerpt": window[:280],
        }

    # Known OCR miss in this source around B4-1 FAR line.
    if "B4-1" not in out and "B1-1" in out:
        source = out["B1-1"]
        out["B4-1"] = {
            "basic_far": source["basic_far"],
            "max_far": source["max_far"],
            "max_far_note": "Recovered from B1-1 suffix-equivalent FAR due OCR miss in B4-1 line",
            "source_method": "same_as_reference",
            "source_section": "8.5",
            "source_excerpt": "B4-1 FAR line contains OCR noise ('arear ratio'); value inferred from B1-1/B4 family mapping.",
        }

    return out


def extract_lot_area_metrics(collapsed_text: str) -> tuple[dict[str, dict], set[str]]:
    lot_map: dict[str, dict] = {}
    no_requirement_codes: set[str] = set()

    direct_re = re.compile(
        r"In\s+(?:an?|the)\s+([A-Z0-9\.-]+)\s+District,\s+there\s+shall\s+be\s+provided\s+not\s+less\s+than\s+([0-9,]+)\s+square\s+feet\s+of\s+lot\s+area\s+per\s+dwelling\s+unit",
        flags=re.IGNORECASE,
    )

    for match in direct_re.finditer(collapsed_text):
        code = normalize_code(match.group(1))
        if not code:
            continue
        sqft = int(match.group(2).replace(",", ""))
        lot_map[code] = {
            "min_lot_area_per_dwelling_unit_sqft": sqft,
            "source_method": "direct_text",
            "source_section": "7.5" if code.startswith("R") else ("8.6" if code.startswith("B") else "9.6"),
            "source_excerpt": collapsed_text[match.start() : match.start() + 260],
        }

    same_as_re = re.compile(
        r"In\s+(?:an?|the)\s+([A-Z0-9\.-]+)\s+District,\s+the\s+minimum\s+requirements\s+for\s+lot\s+area\s+per\s+dwelling\s+unit\s+shall\s+be\s+the\s+same\s+as\s+in\s+a\s+([A-Z0-9\.-]+)\s+District",
        flags=re.IGNORECASE,
    )

    pending_same_as: list[tuple[str, str, str]] = []
    for match in same_as_re.finditer(collapsed_text):
        code = normalize_code(match.group(1))
        ref = normalize_code(match.group(2))
        if code and ref:
            pending_same_as.append((code, ref, collapsed_text[match.start() : match.start() + 260]))

    # Resolve direct same-as references.
    changed = True
    while changed:
        changed = False
        for code, ref, excerpt in pending_same_as:
            if code in lot_map:
                continue
            if ref in lot_map:
                lot_map[code] = {
                    "min_lot_area_per_dwelling_unit_sqft": lot_map[ref]["min_lot_area_per_dwelling_unit_sqft"],
                    "source_method": "same_as_reference",
                    "source_section": "8.6" if code.startswith("B") else "9.6",
                    "source_excerpt": excerpt,
                }
                changed = True

    no_req_re = re.compile(r"In\s+(?:an?|the)\s+([A-Z0-9\.-]+)\s+District,\s+no\s+requirement", flags=re.IGNORECASE)
    for match in no_req_re.finditer(collapsed_text):
        code = normalize_code(match.group(1))
        if code:
            no_requirement_codes.add(code)
            lot_map[code] = {
                "min_lot_area_per_dwelling_unit_sqft": None,
                "source_method": "direct_text",
                "source_section": "8.6" if code.startswith("B") else "9.6",
                "source_excerpt": collapsed_text[match.start() : match.start() + 200],
            }

    # OCR-safe suffix fallback for families that defer to B1-x rules.
    for family in ["B2", "B3", "B4", "B5", "C1", "C2", "C3"]:
        for suffix in ["1", "2", "3", "4", "5"]:
            code = f"{family}-{suffix}"
            ref = f"B1-{suffix}"
            if code in lot_map or code in no_requirement_codes:
                continue
            if ref in lot_map and lot_map[ref]["min_lot_area_per_dwelling_unit_sqft"] is not None:
                lot_map[code] = {
                    "min_lot_area_per_dwelling_unit_sqft": lot_map[ref]["min_lot_area_per_dwelling_unit_sqft"],
                    "source_method": "same_as_reference",
                    "source_section": "8.6" if code.startswith("B") else "9.6",
                    "source_excerpt": f"Suffix fallback from {ref} due OCR noise in same-as line.",
                }

    return lot_map, no_requirement_codes


def parse_crosswalk(path: str) -> pd.DataFrame:
    df = pd.read_csv(path, dtype=str).fillna("")
    df["old_code"] = df["old_code"].map(normalize_code)
    df["new_code"] = df["new_code"].map(normalize_code)
    return df


def infer_c5_from_crosswalk(
    far_map: dict[str, dict],
    lot_map: dict[str, dict],
    crosswalk_df: pd.DataFrame,
) -> tuple[set[str], set[str]]:
    inferred_far: set[str] = set()
    inferred_lot: set[str] = set()

    for _, row in crosswalk_df.iterrows():
        old_code = row.get("old_code")
        new_code = row.get("new_code")
        scope = str(row.get("conversion_scope") or "").strip().lower()

        if scope != "outside_downtown":
            continue
        if pd.isna(old_code) or pd.isna(new_code):
            continue
        old_code = str(old_code).strip()
        new_code = str(new_code).strip()
        if not old_code or not new_code:
            continue
        if not re.fullmatch(r"C5-[1-4]", old_code):
            continue

        if old_code not in far_map and new_code in far_map:
            src = far_map[new_code]
            far_map[old_code] = {
                "basic_far": src["basic_far"],
                "max_far": src.get("max_far"),
                "max_far_note": f"Crosswalk inferred from {new_code}",
                "source_method": "crosswalk_inferred",
                "source_section": "17-1-1406-A",
                "source_excerpt": f"{old_code} -> {new_code} (outside_downtown)",
            }
            inferred_far.add(old_code)

        if old_code not in lot_map and new_code in lot_map:
            src = lot_map[new_code]
            lot_map[old_code] = {
                "min_lot_area_per_dwelling_unit_sqft": src["min_lot_area_per_dwelling_unit_sqft"],
                "source_method": "crosswalk_inferred",
                "source_section": "17-1-1406-A",
                "source_excerpt": f"{old_code} -> {new_code} (outside_downtown)",
            }
            inferred_lot.add(old_code)

    return inferred_far, inferred_lot


def build_unit_restrictions(target_districts: list[str]) -> dict[str, str | None]:
    out: dict[str, str | None] = {code: None for code in target_districts}

    residence_rule = (
        "Gross residential floor area per dwelling unit must be at least 500 sq ft; "
        "lodging room counts as 0.75 dwelling units."
    )
    for code in target_districts:
        if code.startswith("R"):
            out[code] = residence_rule

    for code in target_districts:
        if re.fullmatch(r"B1-[1-5]", code):
            out[code] = (
                "Dwelling units and lodging rooms are not permitted below the second floor; "
                "business uses are not permitted above the ground floor."
            )
        elif re.fullmatch(r"B[23]-[1-5]", code):
            out[code] = "Dwelling units and lodging rooms are not permitted below the second floor."
        elif re.fullmatch(r"B[45]-[1-5]", code):
            out[code] = "Dwelling units and lodging rooms (other than in transient hotel/motel) are not permitted below the second floor."
        elif code in {"B6-6", "B6-7"}:
            out[code] = "Dwelling units and lodging rooms are not permitted below the second floor."
        elif re.fullmatch(r"B7-[567]", code):
            out[code] = "Dwelling units and lodging rooms (other than in transient hotel/motel) are not permitted below the second floor."

    business_density_rule = (
        "Gross residential floor area per dwelling unit must be at least 500 sq ft, "
        "except B6-6, B6-7, B7-6 and B7-7; lodging room counts as 0.5 dwelling units."
    )
    for code in target_districts:
        if code.startswith("B"):
            out[code] = " ".join(filter(None, [out.get(code), business_density_rule]))

    for code in target_districts:
        if re.fullmatch(r"C[123]-[1-7]", code):
            out[code] = "Dwelling units and lodging rooms (other than in transient hotel/motel) are not permitted below the second floor."
        elif code == "C4":
            out[code] = "Dwelling units for watchmen and their families are permitted on premises."

    commercial_density_rule = (
        "Gross residential floor area per dwelling unit must be at least 500 sq ft, "
        "except C3-6 and C3-7; lodging room counts as 0.5 dwelling units."
    )
    for code in target_districts:
        if code.startswith("C"):
            out[code] = " ".join(filter(None, [out.get(code), commercial_density_rule]))

    for code in target_districts:
        if code.startswith("M"):
            out[code] = "Dwelling units for watchmen and their families are permitted on premises."

    return out


def build_old_lookup(
    in_old_zoning_txt: str,
    in_crosswalk_csv: str,
) -> tuple[pd.DataFrame, pd.DataFrame, dict]:
    target_districts = build_target_universe()
    target_set = set(target_districts)

    raw_text = Path(in_old_zoning_txt).read_text(encoding="utf-8", errors="ignore")
    trimmed_text = trim_to_ordinance_text(raw_text)
    normalized_text = normalize_ocr_text(trimmed_text)
    collapsed_text = collapse_text_for_regex(trimmed_text)

    mention_df = parse_mentions(normalized_text.splitlines(), target_set=target_set)

    far_map = extract_far_metrics(collapsed_text)
    lot_map, no_requirement_codes = extract_lot_area_metrics(collapsed_text)
    crosswalk_df = parse_crosswalk(in_crosswalk_csv)
    inferred_far_codes, inferred_lot_codes = infer_c5_from_crosswalk(far_map=far_map, lot_map=lot_map, crosswalk_df=crosswalk_df)

    restrictions = build_unit_restrictions(target_districts)

    airport_note = (
        "No district-wide numeric max height found in targeted sections. "
        "Sec. 5.9 adds airport-area caps (e.g., 25 ft within 7,500 ft of Midway; 150 ft in wider bands)."
    )
    m_boundary_note = (
        "Sec. 10.13 adds residence-boundary controls for M districts, including a 35 ft trigger with distance-based setbacks."
    )

    rows: list[dict] = []
    unresolved_codes: list[str] = []

    for code in target_districts:
        far_info = far_map.get(code)
        lot_info = lot_map.get(code)

        basic_far = far_info["basic_far"] if far_info else None
        max_far = far_info["max_far"] if far_info else None
        max_far_note = far_info["max_far_note"] if far_info else None

        lot_sqft = lot_info["min_lot_area_per_dwelling_unit_sqft"] if lot_info else None

        source_method = "unresolved"
        source_section = ""
        source_excerpt = ""
        if far_info:
            source_method = far_info.get("source_method", "direct_text")
            source_section = far_info.get("source_section", "")
            source_excerpt = far_info.get("source_excerpt", "")
        elif lot_info:
            source_method = lot_info.get("source_method", "direct_text")
            source_section = lot_info.get("source_section", "")
            source_excerpt = lot_info.get("source_excerpt", "")

        if code in inferred_far_codes or code in inferred_lot_codes:
            source_method = "crosswalk_inferred"
            source_section = "17-1-1406-A"
            if not source_excerpt:
                source_excerpt = "Inferred via 2004 conversion crosswalk."

        max_height_notes = airport_note
        if code.startswith("M"):
            max_height_notes = f"{airport_note} {m_boundary_note}"

        if basic_far is None and lot_sqft is None:
            unresolved_codes.append(code)

        rows.append(
            {
                "district_code": code,
                "district_family": district_family(code),
                "effective_start_date": "1957-05-29",
                "effective_end_date": "2004-10-31",
                "basic_far": basic_far,
                "max_far": max_far,
                "max_far_note": max_far_note,
                "min_lot_area_per_dwelling_unit_sqft": lot_sqft,
                "unit_restrictions_text": restrictions.get(code),
                "max_building_height": None,
                "max_height_notes": max_height_notes,
                "source_method": source_method,
                "source_section": source_section,
                "source_excerpt": source_excerpt,
            }
        )

    bulk_df = pd.DataFrame(rows)

    mention_out = mention_df.copy()
    mention_out["extracted_basic_far"] = mention_out["district_code"].map(
        lambda c: far_map.get(c, {}).get("basic_far") if isinstance(c, str) else None
    )
    mention_out["extracted_lot_area_per_dwelling_unit_sqft"] = mention_out["district_code"].map(
        lambda c: lot_map.get(c, {}).get("min_lot_area_per_dwelling_unit_sqft") if isinstance(c, str) else None
    )

    summary = {
        "generated_at_utc": datetime.now(timezone.utc).isoformat(timespec="seconds").replace("+00:00", "Z"),
        "input_old_zoning_txt": in_old_zoning_txt,
        "input_crosswalk_csv": in_crosswalk_csv,
        "target_district_count": len(target_districts),
        "output_rows": int(len(bulk_df)),
        "basic_far_non_null": int(pd.to_numeric(bulk_df["basic_far"], errors="coerce").notna().sum()),
        "max_far_non_null": int(pd.to_numeric(bulk_df["max_far"], errors="coerce").notna().sum()),
        "lot_area_non_null": int(pd.to_numeric(bulk_df["min_lot_area_per_dwelling_unit_sqft"], errors="coerce").notna().sum()),
        "crosswalk_inferred_far_codes": sorted(inferred_far_codes),
        "crosswalk_inferred_lot_codes": sorted(inferred_lot_codes),
        "no_requirement_codes": sorted(no_requirement_codes),
        "unresolved_codes": unresolved_codes,
        "c5_5_unresolved": bool(
            bulk_df.loc[bulk_df["district_code"] == "C5-5", "basic_far"].isna().all()
            and bulk_df.loc[bulk_df["district_code"] == "C5-5", "min_lot_area_per_dwelling_unit_sqft"].isna().all()
        ),
        "mentions_target_rows": int(mention_out[mention_out["is_target_universe"] == True].shape[0]),
        "mentions_target_with_hits": int(
            mention_out.loc[mention_out["is_target_universe"] == True, "mentioned_in_text"].fillna(False).sum()
        ),
    }

    return bulk_df, mention_out, summary


def main() -> int:
    args = parse_args()

    bulk_df, _, _ = build_old_lookup(
        in_old_zoning_txt=args.in_old_zoning_txt,
        in_crosswalk_csv=args.in_crosswalk_csv,
    )

    ensure_parent(args.out_bulk_csv)
    bulk_df.to_csv(args.out_bulk_csv, index=False)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
