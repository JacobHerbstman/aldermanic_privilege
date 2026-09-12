# Historical construction coordinates

Match missing construction locations to the recorded historical parcel and address records. Retain the approved identity checks, address corrections, year window, and distance limits.

Run `make` from `tasks/construction_historical_coordinates/code/`. Its Makefile lists each input and its producer.

- [match_historical_parcel_records.R](code/match_historical_parcel_records.R): `density_historical_building_universe.csv`, `density_historical_coordinate_candidates.csv`, `density_project_lineage.csv`.
- [select_historical_coordinates.R](code/select_historical_coordinates.R): `density_parcel_address_selected_history.csv`, `density_parcel_address_lineage_evidence.csv`, `density_historical_coordinates.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
