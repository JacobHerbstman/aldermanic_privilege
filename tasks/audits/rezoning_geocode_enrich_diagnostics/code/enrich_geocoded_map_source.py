#!/usr/bin/env python3
import argparse
from pathlib import Path
import sys

import pandas as pd

PRODUCTION_CODE = Path(__file__).resolve().parents[3] / "rezoning_geocode_enrich" / "code"
if str(PRODUCTION_CODE) not in sys.path:
    sys.path.insert(0, str(PRODUCTION_CODE))

from enrich_geocoded_rezonings import save_scatter_map

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--date-tag", default="19990101_20260212")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    tag = args.date_tag
    df = pd.read_csv(Path("../input") / f"rezoning_geocoded_enriched_{tag}.csv", dtype=str, low_memory=False)
    warning = save_scatter_map(
        df,
        color_col="geocode_source",
        color_map={
            "parcel_match": "#1f77b4",
            "chicago_geocoder": "#2ca02c",
            "census_geocoder": "#ff7f0e",
            "unmatched": "#d62728",
            "unknown": "#7f7f7f",
        },
        title="Geocoded Rezonings by Source",
        out_path=Path("../output") / f"map_geocode_source_{tag}.pdf",
    )
    if warning:
        print(warning)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
