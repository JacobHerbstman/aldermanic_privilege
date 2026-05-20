# calculate_sale_distances

Purpose: Calculates sale distances.

The output preserves nominal transaction prices as `sale_price_nominal`, deflates them to 2022 dollars using the monthly Chicago all-items CPI-U deflator from `download_fred_cpi`, winsorizes the real price at the 1st and 99th percentiles, and stores that real analysis price as `sale_price`.

Produces: Produces files such as `output/sales_geometry_diagnostics.csv`, `output/sales_pre_scores.csv`, `output/sales_with_ward_distances.csv` and related task-specific outputs in `output/`.

Approx. runtime: ~1-10 minutes.
