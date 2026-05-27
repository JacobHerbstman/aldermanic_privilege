# calculate_rent_distances

Purpose: Calculates RentHub floorplan-month distances to the nearest in-ward ward boundary.

The output preserves nominal listed rents as `rent_price_nominal`, then replaces `rent_price` with listed rent in 2022 dollars using the monthly Chicago all-items CPI-U deflator from `download_fred_cpi`. Downstream rental RD and balance tasks therefore use real listed rents whenever they model `log(rent_price)`.

The task uses audited RentHub geometry coordinates from `renthub_proxy_consistency_audit`: stable address-location groups are standardized to their primary location, while unstable address locations remain flagged for downstream RD balance checks.

The script still runs geometry contract and ward-hit checks before writing the handoff. Diagnostic CSV exports are opt-in via `WRITE_RENT_DISTANCE_DIAGNOSTICS=1`.

Produces:
- `output/rent_pre_scores_full.parquet`

Approx. runtime: ~1-10 minutes.
