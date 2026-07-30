# renthub_location_corrections

Purpose: Produces the location corrections and location-quality flags used by the rental analysis.

Address stems are normalized before grouping. Coordinate clusters within an address are grouped at 200ft. The primary coordinate is used when its cluster contains at least 85% of the address's raw rows and no secondary cluster contains at least 10% of rows more than 500ft away. Verified coordinates from `renthub_location_hand_adjudications` take priority.

This task does not filter listings or create general RentHub diagnostics.

Produces: `output/chicago_rent_panel_location_corrections.parquet`.
