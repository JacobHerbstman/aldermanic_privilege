import argparse
import re
from pathlib import Path

import pandas as pd

BAD_ADDR = {
    "",
    "nan",
    "na",
    "none",
    "nan, chicago, il",
    "na, chicago, il",
    "none, chicago, il",
    ", chicago, il",
    "chicago, il",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--in-unmatched-csv", required=True)
    parser.add_argument("--out-census-template-csv", required=True)
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def clean_street_address(value) -> str:
    if pd.isna(value):
        return ""
    out = str(value).strip()
    out = re.sub(r"\s+", " ", out)
    return out


def derive_from_external(address_for_external) -> str:
    if pd.isna(address_for_external):
        return ""
    text = str(address_for_external).strip()
    lowered = text.lower()
    if lowered in BAD_ADDR:
        return ""
    text = re.sub(r",?\s+chicago\s*(,\s*)?il(?:linois)?\s*$", "", text, flags=re.IGNORECASE)
    text = re.sub(r"\s+", " ", text).strip(" ,")
    return text


def main() -> int:
    args = parse_args()

    unmatched = pd.read_csv(args.in_unmatched_csv, dtype=str, low_memory=False)

    out = unmatched[["external_row_id", "street_address", "city", "state", "zip", "address_for_external"]].copy()
    out["street_address"] = out["street_address"].map(clean_street_address)

    missing = out["street_address"].str.strip().eq("")
    out.loc[missing, "street_address"] = out.loc[missing, "address_for_external"].map(derive_from_external)

    out["city"] = out["city"].fillna("Chicago")
    out["state"] = out["state"].fillna("IL")
    out["zip"] = out["zip"].fillna("")

    template = out[["external_row_id", "street_address", "city", "state", "zip"]].copy()

    ensure_parent(args.out_census_template_csv)
    template.to_csv(args.out_census_template_csv, index=False, header=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
