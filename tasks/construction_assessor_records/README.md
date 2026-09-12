# Assessor construction records

Read the recorded residential and commercial Assessor files, remove empty residential cards, select the existing source snapshots, and attach the recorded parcel coordinates.

Run `make` from `tasks/construction_assessor_records/code/`. Its Makefile lists each input and its producer.

- [prepare_construction_records.R](code/prepare_construction_records.R): `residential_cross_section.csv`, `residential_discovery_cross_section.csv`, `multifamily_data_cleaned.csv`.
- [geocode_residential_data.R](code/geocode_residential_data.R): `geocoded_residential_data.gpkg`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
