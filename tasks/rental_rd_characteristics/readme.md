# rental_rd_characteristics

Purpose: Builds rental boundary samples with listing, housing, and amenity controls for a requested bandwidth.

Inputs:
- `input/rent_with_ward_distances.parquet`: RentHub floorplan-month panel with ward-pair distances, score side assignment, and segment IDs.
- Amenity layers from `prepare_amenities_data` and the Lake Michigan water shapefile.

Produces: `output/rental_rd_characteristics_panel_bw<bandwidth>.parquet`.

The default bandwidth is 500 feet. The price-placebo task requests the same
panel at 1,500 feet so it can move the cutoff 1,000 feet into either ward.
