# Historical construction parcels

Select construction-year parcels from the recorded Cook County responses. Parcel geometry supplies identity and location evidence; density uses source-reported land area.

Run `make` from `tasks/construction_historical_sites/code/`. Its Makefile lists each input and its producer.

- [build_historical_project_geography.R](code/build_historical_project_geography.R): `historical_project_parcel_requests.csv`, `historical_project_parcel_coverage.csv`, `missing_project_reference_points.csv`, `historical_project_predecessor_resolution.csv`, `historical_project_year_geometry.gpkg`.
- [select_project_parcels.R](code/select_project_parcels.R): `preferred_project_geography_requests.csv`, `preferred_historical_parcels.gpkg`, `preferred_historical_parcel_coverage.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
