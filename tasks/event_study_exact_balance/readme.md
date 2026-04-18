# event_study_exact_balance

Purpose: Builds exact-distance parcel baselines for 2010 census blocks and uses them to compare treated and control blocks in the 2015 permit and home-sales event-study samples.

Produces: Produces files such as `output/block_parcel_baselines_2014.csv`, `output/permit_exact_balance_summary.tex`, `output/sales_exact_balance_summary.tex`, and related task-specific outputs in `output/`.

Approximate runtime: 5-15 minutes, depending on whether the parcel-to-amenity baseline needs to be rebuilt from scratch.
