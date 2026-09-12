# Commercial new-building evidence

Compare the recorded commercial building sites with City footprints and permits to distinguish new buildings from older structures. These calculations support the existing commercial source-selection rules.

Run `make` from `tasks/construction_commercial_permits/code/`. Its Makefile lists each input and its producer.

- [identify_commercial_new_buildings.R](code/identify_commercial_new_buildings.R): `commercial_city_building_footprints.gpkg`, `commercial_ground_up_evidence.csv`.
- [build_commercial_address_permit_history.R](code/build_commercial_address_permit_history.R): `commercial_address_permit_history.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
