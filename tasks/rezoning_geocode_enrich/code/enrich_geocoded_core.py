#!/usr/bin/env python3
import argparse

from enrich_geocoded_rezonings import build_enriched_dataframe, ensure_parent

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--date-tag", default="20101101_20201231")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    tag = args.date_tag
    out_csv = f"../output/rezoning_geocoded_enriched_{tag}.csv"
    df, _, _, _ = build_enriched_dataframe(
        in_geocoded_csv=f"../input/rezoning_geocode_with_external_{tag}.csv",
        in_zoning_lookup_csv="../input/zoning_far_lookup_clean.csv",
        in_ward_gpkg="../input/ward_panel.gpkg",
    )
    ensure_parent(out_csv)
    df.to_csv(out_csv, index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
