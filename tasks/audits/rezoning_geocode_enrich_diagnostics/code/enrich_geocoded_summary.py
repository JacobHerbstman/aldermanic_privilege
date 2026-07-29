#!/usr/bin/env python3
import argparse
from pathlib import Path
import sys

import pandas as pd

PRODUCTION_CODE = Path(__file__).resolve().parents[3] / "rezoning_geocode_enrich" / "code"
if str(PRODUCTION_CODE) not in sys.path:
    sys.path.insert(0, str(PRODUCTION_CODE))

from enrich_geocoded_rezonings import build_sponsor_validation, build_ward_counts, ensure_parent

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--date-tag", default="19990101_20260212")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    tag = args.date_tag
    in_csv = Path("../input") / f"rezoning_geocoded_enriched_{tag}.csv"
    ward_counts_csv = Path("../output") / f"ward_rezoning_counts_{tag}.csv"
    sponsor_validation_csv = Path("../output") / f"sponsor_ward_validation_{tag}.csv"

    df = pd.read_csv(in_csv, dtype=str, low_memory=False)
    ward_counts = build_ward_counts(df)
    sponsor_validation = build_sponsor_validation(df)

    ensure_parent(ward_counts_csv)
    ensure_parent(sponsor_validation_csv)
    ward_counts.to_csv(ward_counts_csv, index=False)
    sponsor_validation.to_csv(sponsor_validation_csv, index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
