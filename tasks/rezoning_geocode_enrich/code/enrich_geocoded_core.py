#!/usr/bin/env python3
import argparse

from enrich_geocoded_rezonings import build_enriched_dataframe, ensure_parent

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--date-tag", default="19990101_20260212")
    parser.add_argument("--pd-far-review-start-year", type=int, default=2011)
    parser.add_argument("--pd-far-review-end-year", type=int, default=2020)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    tag = args.date_tag
    review_tag = f"{args.pd_far_review_start_year}_{args.pd_far_review_end_year}"
    out_csv = f"../output/rezoning_geocoded_enriched_{tag}.csv"
    df, _, _, _ = build_enriched_dataframe(
        in_geocoded_csv=f"../input/rezoning_geocode_with_external_{tag}.csv",
        in_zoning_lookup_csv="../input/zoning_far_lookup_clean.csv",
        in_ward_gpkg="../input/ward_panel.gpkg",
        in_pd_far_updates_csv=f"../input/pd_far_text_review_updates_{review_tag}.csv",
    )
    ensure_parent(out_csv)
    df.to_csv(out_csv, index=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
