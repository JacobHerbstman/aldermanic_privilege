# Construction data: execution order

Generated from the current Makefile prerequisites. Scripts at the same level are independent.

## Dependency level 1

- [tasks/prepare_new_construction/code/select_assessor_buildings.R](../tasks/prepare_new_construction/code/select_assessor_buildings.R): `assessor_buildings.csv`, `assessor_measurement_records.csv`, `building_permit_evidence.csv`, `building_permits_for_verification.gpkg`

## Dependency level 2

- [tasks/prepare_new_construction/code/build_construction_data.R](../tasks/prepare_new_construction/code/build_construction_data.R): `preferred_new_construction_boundary_scope.csv`, `preferred_new_construction_project_ledger.csv`

## Dependency level 3

- [tasks/new_construction_analysis_data/code/attach_construction_regressors.R](../tasks/new_construction_analysis_data/code/attach_construction_regressors.R): `construction_regressors.csv`

## Dependency level 4

- [tasks/new_construction_analysis_data/code/build_new_construction_analysis_data.R](../tasks/new_construction_analysis_data/code/build_new_construction_analysis_data.R): `new_construction_analysis_data.csv`
