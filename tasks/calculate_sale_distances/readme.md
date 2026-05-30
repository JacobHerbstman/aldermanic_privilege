# calculate_sale_distances

Purpose: Calculates the sale-distance handoff used by downstream sales and event-study tasks.

The output preserves nominal transaction prices as `sale_price_nominal`, deflates them to 2022 dollars using the monthly Chicago all-items CPI-U deflator from `download_fred_cpi`, winsorizes the real price at the 1st and 99th percentiles, and stores that real analysis price as `sale_price`.

Produces:
- `output/sales_pre_scores.csv`

The script keeps only the production handoff. Geometry and ward-assignment checks run inline before the CSV is written; diagnostic reports live in audit tasks.

Approx. runtime: ~1-10 minutes.
