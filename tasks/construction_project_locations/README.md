# Construction project locations

Match the saved address-geocoder and predecessor-parcel responses, select project locations, and calculate preliminary ward-boundary coverage. Read the recorded source vintage without making new geocoder requests.

Run `make` from `tasks/construction_project_locations/code/`. Its Makefile lists each input and its producer.

- [match_project_addresses_and_predecessors.R](code/match_project_addresses_and_predecessors.R): `preferred_address_geocode_requests.csv`, `preferred_historical_address_geocodes.csv`, `preferred_chicago_address_geocodes.csv`, `preferred_predecessor_reference_points.csv`, `preferred_historical_predecessor_selected.gpkg`, `preferred_historical_predecessor_resolution.csv`.
- [locate_construction_projects.R](code/locate_construction_projects.R): `preferred_project_year_geometry.gpkg`, `preferred_project_component_geometry.gpkg`, `preferred_project_year_centroids.gpkg`, `preferred_project_year_geometry_coverage.csv`, `preferred_project_boundary_scope.csv`, `preferred_adjudication_scope.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
