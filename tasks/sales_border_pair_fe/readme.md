# sales_border_pair_fe

Purpose: Handles sales border pair FE.

The task reads the scored sales panel where `sale_price` is the winsorized 2022-dollar analysis price and `sale_price_nominal` preserves the source-dollar value for diagnostics.

Produces:
- `output/sales_with_hedonics_amenities.parquet`
- `output/sales_rd_flat_bw500_year_quarter_amenity_clust_segment.pdf`

Approx. runtime: ~2-15 minutes.
