# rental_rd_characteristics

Purpose: Builds the 500 ft rental RD characteristics panel with listing, hedonic, and amenity-distance controls.

Inputs:
- `input/rent_with_ward_distances.parquet`: RentHub floorplan-month panel with ward-pair distances, score side assignment, and segment IDs.
- Amenity layers from `prepare_amenities_data` and the Lake Michigan water shapefile.

Produces: `output/rental_rd_characteristics_panel_bw500.parquet`.
