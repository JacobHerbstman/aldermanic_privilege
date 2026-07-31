# Clean Building Permits

This task parses permit dates and numeric fields, recovers coordinates from the
City's projected coordinates when latitude and longitude are missing, removes
invalid locations, and classifies the permit groups used in the paper. It keeps
applications from 2006 through 2022 with nonnegative processing times and
writes `output/building_permits_clean.gpkg` in EPSG:3435.
