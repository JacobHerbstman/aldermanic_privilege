# price_rd_stress_tests

Purpose: Paper-facing placebo RD plots for listed rents and home sales.

Inputs:
- `input/rent_with_ward_distances_full.parquet`: listed-rent RD panel with ward-boundary distances.
- `input/sales_with_hedonics_amenities.parquet`: sales RD panel with audited geometry, real prices, hedonics, and amenities.
- Amenity layers from `prepare_amenities_data` and the OSM water shapefile.

Main outputs:
- `output/rent_placebo_rd_main_style.pdf`
- `output/sales_placebo_rd_main_style.pdf`

Notes:
- The older supply-mechanism stress-test diagnostics now live in `tasks/audits/price_rd_stress_tests_audit`.
