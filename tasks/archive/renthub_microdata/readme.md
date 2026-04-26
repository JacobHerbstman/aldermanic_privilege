# renthub_microdata

Purpose: Builds the canonical citywide RentHub microdataset from raw RentHub parquet shards.

Produces:
- `output/renthub_clean_rows.parquet`
- `output/renthub_listing_episodes.parquet`
- `output/renthub_repeat_pairs.parquet`
- microdata diagnostics and trend-validation CSVs in `output/`

Approx. runtime: ~5-20 minutes, depending on available caches and spatial joins.
