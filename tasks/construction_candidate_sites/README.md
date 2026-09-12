# Construction candidate sites

Prepare the historical ward maps and calculate preliminary parcel-to-boundary distances used to identify candidate buildings.

Run `make` from `tasks/construction_candidate_sites/code/`. Its Makefile lists each input and its producer.

- [build_construction_discovery_geography.R](code/build_construction_discovery_geography.R): `construction_discovery_ward_maps.gpkg`, `construction_discovery_boundaries.gpkg`.
- [build_construction_parcel_distances.R](code/build_construction_parcel_distances.R): `construction_parcel_boundary_distances.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
