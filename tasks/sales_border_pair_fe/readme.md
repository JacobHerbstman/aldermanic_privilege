# sales_border_pair_fe

Purpose: Handles sales border pair FE.

The task reads the scored sales panel where `sale_price` is the winsorized 2022-dollar analysis price and `sale_price_nominal` preserves the source-dollar value for diagnostics.

Produces: Produces files such as `output/fe_table_sales_right_bw500_year_quarter_amenity_clust_segment.csv`, `output/fe_table_sales_right_bw500_year_quarter_amenity_clust_segment.tex`, the matching ward-pair-clustered diagnostic table, and related task-specific outputs in `output/`.

Approx. runtime: ~2-15 minutes.
