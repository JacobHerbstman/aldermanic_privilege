# rental_rd_characteristics

Purpose: Builds rental boundary samples with listing, housing, and amenity controls for a requested bandwidth.

Inputs:
- `input/rent_with_ward_distances.parquet`: RentHub floorplan-month panel with ward-pair distances, score side assignment, and segment IDs.
- Amenity layers from `prepare_amenities_data` and the Lake Michigan water shapefile.

Produces: `output/rental_rd_characteristics_panel_bw<bandwidth>.parquet`.

The default bandwidth is 1,500 feet so the paper can estimate the main
500-foot comparison and move the cutoff 1,000 feet into either ward.
