# Construction permit matches

Read the complete recorded permit history, match candidate buildings by parcel or historical geometry, and connect permit revisions. Preserve the original matching and dwelling-count rules.

Run `make` from `tasks/construction_project_permits/code/`. Its Makefile lists each input and its producer.

- [match_construction_permits.R](code/match_construction_permits.R): `building_permits_for_verification.gpkg`, `new_construction_exact_permit_matches.csv`, `new_construction_permit_unit_mentions.csv`, `new_construction_project_components.csv`.
- [match_spatial_permits_and_revisions.R](code/match_spatial_permits_and_revisions.R): `new_construction_spatial_permit_matches.csv`, `project_permit_chain_links.csv`, `project_permit_chain_unit_mentions.csv`, `project_permit_chain_summary.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
