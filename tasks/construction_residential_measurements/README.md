# Residential building measurements

Select one complete assessment for each residential building, apply the committed measurement corrections, and resolve completed condo records and replacement parent records. Each measurement keeps its recorded source.

Run `make` from `tasks/construction_residential_measurements/code/`. Its Makefile lists each input and its producer.

- [select_and_correct_residential_measurements.R](code/select_and_correct_residential_measurements.R): `residential_selected_assessments.csv`, `preferred_residential_project_candidates.csv`, `preferred_residential_project_components.csv`, `residential_adjudication_queue.csv`.
- [select_residential_buildings.R](code/select_residential_buildings.R): `residential_successor_condo_requests.csv`, `residential_review_current_parcel_links.csv`, `residential_tieback_episode_resolution.csv`, `residential_class297_resolution.csv`, `residential_class297_source_disposition.csv`, `residential_overlap_resolution.csv`, `residential_selected_buildings.csv`, `residential_review_source_dispositions.csv`.

Reviewed decisions come from [new_construction_cleaning](../new_construction_cleaning/README.md).
Data reports are written when the datasets are saved. They are not Make targets.
