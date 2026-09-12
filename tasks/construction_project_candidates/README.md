# Candidate construction buildings

Identify residential cards and tied parcels, and commercial source families. Preserve the component records used to determine whether records describe the same building.

Run `make` from `tasks/construction_project_candidates/code/`. Its Makefile lists each input and its producer.

- [build_residential_project_candidates.R](code/build_residential_project_candidates.R): `residential_project_candidate_inventory.csv`, `residential_project_history_summary.csv`, `residential_fractional_base_groups.csv`, `residential_tieback_groups_full.csv`, `residential_tieback_members_full.csv`, `residential_multicard_cards.csv`.
- [build_commercial_project_candidates.R](code/build_commercial_project_candidates.R): `commercial_entity_version_candidates.csv`, `commercial_address_family_candidates.csv`, `commercial_entity_component_candidates.csv`, `commercial_family_vintage_summary.csv`, `commercial_production_family_members.csv`, `commercial_project_family_review.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
