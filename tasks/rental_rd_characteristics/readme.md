# rental_rd_characteristics

Purpose: Decomposes the RentHub 500ft RD rent jump into observable listing, quality, and amenity differences using the current cleaned RentHub panel and audited rental geography.

Inputs:
- `input/rent_with_ward_distances.parquet`: current RentHub floorplan-month panel with ward-pair distances, score side assignment, segment IDs, and location-quality flags.
- Amenity layers from `prepare_amenities_data` and the Lake Michigan water shapefile.

Outputs:
- `output/rental_rd_characteristics_panel_bw500.parquet`: RD-window analysis panel with amenity distances computed from the same corrected coordinates used by the rental distance task.
- `output/rental_rd_covariate_balance_bw500.csv`: segment-month FE discontinuities in hedonic, quality, and amenity covariates.
- `output/rental_rd_covariate_balance_bw500.pdf`: standardized covariate-balance plot for the main and clean-location samples.
- `output/rental_rd_rent_attenuation_bw500.csv`: common-sample rent jump estimates with no controls, hedonic controls, and hedonic plus amenity controls.
- `output/rental_rd_rent_attenuation_bw500.pdf`: coefficient plot for the attenuation table.

The task does not reuse the old rental-characteristics scripts directly because those scripts predate the current RentHub cleaning, location correction, and segment-assignment audits.
