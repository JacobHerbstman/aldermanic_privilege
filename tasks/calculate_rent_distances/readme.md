# calculate_rent_distances

Purpose: Calculates RentHub floorplan-month distances to the nearest in-ward ward boundary.

The output preserves nominal listed rents as `rent_price_nominal`, then replaces `rent_price` with listed rent in 2022 dollars using the monthly Chicago all-items CPI-U deflator from `download_fred_cpi`. Downstream rental RD and balance tasks therefore use real listed rents whenever they model `log(rent_price)`.

The task uses audited RentHub geometry coordinates from `renthub_proxy_consistency_audit`: stable address-location groups are standardized to their primary location, while unstable address locations remain flagged for downstream RD balance checks.

Location-quality outputs:
- `output/rent_rd_location_quality_summary_full.csv` reports questionable-location and modal-coordinate reassignment rates overall, within 1000ft, and within the 500ft rental RD bandwidth.
- `output/rent_rd_location_questionable_addresses_full.csv` lists address stems inside 500ft with unresolved/questionable location flags or modal-coordinate assignment changes.
- `output/rent_modal_coordinate_sensitivity_full.csv` compares raw-coordinate and modal-coordinate ward/pair assignments inside 500ft.
- `output/rent_geometry_contract_audit_full.csv` recomputes 500ft ward, neighbor ward, ward pair, and distance assignments and hard-fails on any mismatch.
- `output/rent_ward_hit_multiplicity_audit_full.csv` reports ward polygon hit counts inside 500ft and hard-fails if any point hits zero or multiple wards.

Produces: Produces files such as `output/rent_geometry_diagnostics_full.csv`, `output/rent_pre_scores_full.parquet` and related task-specific outputs in `output/`.

Approx. runtime: ~1-10 minutes.
