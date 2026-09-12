# Commercial building measurements

Select commercial assessments, apply the adopted source-selection rules, and apply the committed commercial corrections. Preserve the checks that prevent overlapping correction instructions.

Run `make` from `tasks/construction_commercial_measurements/code/`. Its Makefile lists each input and its producer.

- [build_preferred_commercial_candidates.R](code/build_preferred_commercial_candidates.R): `preferred_commercial_project_candidates.csv`, `preferred_commercial_project_components.csv`, `commercial_adjudication_queue.csv`.
- [select_commercial_measurements.R](code/select_commercial_measurements.R): `commercial_post_evidence_resolution.csv`, `preferred_commercial_projects.csv`, `preferred_commercial_source_disposition.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
