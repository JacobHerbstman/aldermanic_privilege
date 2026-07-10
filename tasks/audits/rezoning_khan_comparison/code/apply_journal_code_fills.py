#!/usr/bin/env python3
import argparse
from pathlib import Path

import pandas as pd


REQUIRED_COLUMNS = [
    "matter_id",
    "journal_meeting_date",
    "app_number",
    "from_code_journal",
    "to_code_journal",
    "journal_evidence",
    "journal_note",
]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--date-tag", default="20101101_20160831")
    return parser.parse_args()


def text_series(series: pd.Series) -> pd.Series:
    return series.fillna("").astype(str).str.strip()


def main() -> int:
    args = parse_args()
    matters = pd.read_csv(f"../input/zoning_matters_{args.date_tag}.csv", dtype=str, low_memory=False)
    fills = pd.read_csv("../input/khan_journal_2010_code_fills.csv", dtype=str, low_memory=False)

    missing = [col for col in REQUIRED_COLUMNS if col not in fills.columns]
    if missing:
        raise ValueError(f"Journal fill table is missing columns: {', '.join(missing)}")
    if fills["matter_id"].duplicated().any():
        duplicated = sorted(fills.loc[fills["matter_id"].duplicated(), "matter_id"].astype(str).unique())
        raise ValueError(f"Duplicate journal fill matter_id rows: {duplicated}")
    if matters["matter_id"].duplicated().any():
        duplicated = sorted(matters.loc[matters["matter_id"].duplicated(), "matter_id"].astype(str).unique())
        raise ValueError(f"Duplicate zoning matter rows block journal fills: {duplicated}")

    missing_ids = sorted(set(fills["matter_id"].astype(str)) - set(matters["matter_id"].astype(str)))
    if missing_ids:
        raise ValueError(f"Journal fill rows are missing from zoning matters: {missing_ids}")

    out = matters.copy()
    for col in ["from_zoning_raw", "to_zoning_raw", "from_zoning", "to_zoning"]:
        if col not in out.columns:
            out[col] = pd.NA

    out["journal_code_fill_applied"] = False
    out["journal_from_code_fill"] = pd.NA
    out["journal_to_code_fill"] = pd.NA
    out["journal_code_fill_evidence"] = pd.NA
    out["journal_code_fill_note"] = pd.NA

    fills = fills.set_index(fills["matter_id"].astype(str), drop=False)
    matter_id = out["matter_id"].astype(str)
    fill_mask = matter_id.isin(fills.index)

    existing_from = text_series(out.loc[fill_mask, "from_zoning_raw"])
    existing_to = text_series(out.loc[fill_mask, "to_zoning_raw"])
    if existing_from.ne("").any() or existing_to.ne("").any():
        bad = out.loc[fill_mask & (text_series(out["from_zoning_raw"]).ne("") | text_series(out["to_zoning_raw"]).ne("")), "matter_id"]
        raise ValueError(f"Journal fills would overwrite existing raw zoning fields: {sorted(bad.astype(str).tolist())}")

    out.loc[fill_mask, "journal_code_fill_applied"] = True
    out.loc[fill_mask, "journal_from_code_fill"] = matter_id.loc[fill_mask].map(fills["from_code_journal"])
    out.loc[fill_mask, "journal_to_code_fill"] = matter_id.loc[fill_mask].map(fills["to_code_journal"])
    out.loc[fill_mask, "journal_code_fill_evidence"] = matter_id.loc[fill_mask].map(fills["journal_evidence"])
    out.loc[fill_mask, "journal_code_fill_note"] = matter_id.loc[fill_mask].map(fills["journal_note"])
    out.loc[fill_mask, "from_zoning_raw"] = out.loc[fill_mask, "journal_from_code_fill"]
    out.loc[fill_mask, "to_zoning_raw"] = out.loc[fill_mask, "journal_to_code_fill"]
    out.loc[fill_mask, "from_zoning"] = out.loc[fill_mask, "journal_from_code_fill"]
    out.loc[fill_mask, "to_zoning"] = out.loc[fill_mask, "journal_to_code_fill"]

    Path("../output").mkdir(parents=True, exist_ok=True)
    out.to_csv(f"../output/khan_zoning_matters_journal_codes_{args.date_tag}.csv", index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
