# Residential building identities

Compare residential card histories and identify distinct buildings. Apply recorded identity decisions once and retain separately measured individual homes.

Run `make` from `tasks/construction_residential_identity/code/`. Its Makefile lists each input and its producer.

- [build_residential_tieback_temporal_evidence.R](code/build_residential_tieback_temporal_evidence.R): `residential_tieback_temporal_lineage_evidence.csv`, `residential_tieback_temporal_snapshots.csv`.
- [build_residential_assessor_projects.R](code/build_residential_assessor_projects.R): `residential_assessor_project_candidates.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
