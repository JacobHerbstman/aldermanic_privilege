# RentHub Location Corrections

This task identifies addresses whose reported coordinates are sufficiently
stable for the rental analysis and applies the hand-reviewed coordinates when
needed.

Address stems are normalized before grouping. Coordinates within 200 feet are
treated as one cluster. The largest cluster is used when it contains at least
85% of an address's rows and no second cluster contains at least 10% of the
rows more than 500 feet away. Hand-reviewed coordinates take priority.

The task writes `output/chicago_rent_panel_location_corrections.parquet`. It
does not otherwise filter the rental panel.
