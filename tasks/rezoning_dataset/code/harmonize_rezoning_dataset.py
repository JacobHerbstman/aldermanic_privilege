#!/usr/bin/env python3
import argparse
from pathlib import Path

import pandas as pd

from build_rezoning_dataset import build_harmonized_dataset


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--modern-date-tag", default="20101201_20260212")
    parser.add_argument("--hist-year-tag", default="1999_2010")
    parser.add_argument("--dataset-date-tag", default="19990101_20260212")
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def main() -> int:
    args = parse_args()
    modern_df = pd.read_csv(f"../input/zoning_matters_{args.modern_date_tag}.csv", dtype=str, low_memory=False)
    historical_df = pd.read_csv(f"../input/journal_rezonings_first_pass_{args.hist_year_tag}.csv", dtype=str, low_memory=False)
    out_df, _ = build_harmonized_dataset(modern_df=modern_df, historical_df=historical_df)

    out_csv = f"../output/rezoning_dataset_harmonized_{args.dataset_date_tag}.csv"
    ensure_parent(out_csv)
    out_df.to_csv(out_csv, index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
