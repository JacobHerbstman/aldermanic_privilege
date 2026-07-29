#!/usr/bin/env python3
import argparse
from pathlib import Path

import pandas as pd

from build_rezoning_dataset import build_finalized_outputs


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dataset-date-tag", default="19990101_20260212")
    return parser.parse_args()


def ensure_parent(path: str) -> None:
    Path(path).parent.mkdir(parents=True, exist_ok=True)


def main() -> int:
    args = parse_args()
    tag = args.dataset_date_tag
    enriched_df = pd.read_csv(f"../input/rezoning_geocoded_enriched_{tag}.csv", low_memory=False)
    alderman_panel_df = pd.read_csv("../input/chicago_alderman_panel.csv", dtype=str, low_memory=False)

    final_all, final_usable, _ = build_finalized_outputs(enriched_df, alderman_panel_df=alderman_panel_df)

    out_all = f"../output/rezoning_dataset_all_{tag}.csv"
    out_usable = f"../output/rezoning_dataset_usable_{tag}.csv"
    ensure_parent(out_all)
    ensure_parent(out_usable)
    final_all.to_csv(out_all, index=False)
    final_usable.to_csv(out_usable, index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
