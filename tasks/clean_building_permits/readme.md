# Clean Building Permits

This task parses permit dates and numeric fields, recovers coordinates from the
City's projected coordinates when latitude and longitude are missing, removes
invalid locations, and classifies the permit groups used in the paper. It keeps
applications with nonnegative processing times, for each period of the permit
extracts (`tasks/download_building_permits`), and writes one GeoPackage per
period in EPSG:3435, each with the layer `building_permits_clean`:

- `output/building_permits_clean_2006_2022.gpkg`, the permits the paper uses
  (716,345 records);
- `output/building_permits_clean_2023_2026.gpkg`, applications of January 2023
  through September 28, 2026 (120,383 records). The source lists issued permits
  only, so the last months' applications still under review are missing.

The 2006--2022 file was renamed from `building_permits_clean.gpkg` on September
30, 2026, when the second period was added; its records are unchanged.
