# Recorded construction decisions

This task supplies the decisions already made when reviewing the Assessor data.
`output/` contains versioned source CSVs. There are no R scripts and no recipe
that recreates human judgments. `make` requires the committed source CSVs. Their reports are committed alongside
them; a missing decision CSV must be restored from Git.

## Start here

[construction_modifications.csv](output/construction_modifications.csv) records
308 instructions covering construction years, dwelling units, building area,
and land area. These are instructions, not 308 additional buildings. The file
combines four formerly separate tables without changing their decisions:

| Former table, preserved in the audit archive | Instructions | Where applied |
| --- | ---: | --- |
| residential_reviewed_construction_years.csv | 191 | Before matching construction episodes |
| residential_building_corrections.csv | 55 | After selecting assessment measurements |
| residential_reviewed_land_areas.csv | 2 | After selecting assessment measurements |
| commercial_measurement_corrections.csv | 60 | When selecting the final commercial buildings |

`source_project_id` identifies the original record; `final_project_id` identifies
the retained building. `application_stage` states when the decision applies.
`application_scope` distinguishes a year correction, a land correction, a
complete building decision, or selected fields. Empty measurement fields carry
no replacement instruction; the existing action and density flags govern
whether an incomplete building is retained. Reported values are retained where
the producer checks the old measurement before replacing it. Evidence and
reasons accompany the recorded decisions. Areas are square feet.

The residential year producer applies only `project_identity` rows. The
residential measurement producer reads the remaining residential rows once.
The commercial producer reads only `commercial_measurements` rows and rejects
overlap with its component and whole-project decisions. Geography reads the
original and corrected year solely to match historical records; it does not
apply the year correction again.

[construction_building_types.csv](output/construction_building_types.csv)
contains the 56 applicable building-type decisions from two older review
spreadsheets. Existing project-review priority is resolved in this source table.
Production no longer reads the older spreadsheets' unused years or areas.
The general rule that a finished one-dwelling building is single-family remains
in code.

## Other decisions

The remaining CSVs record different relationships: which Assessor cards form a
building, which duplicate record is replaced, which records are excluded, and
which parcel or permit locates a building. They stay separate so that a parcel
match is not confused with a measurement correction. Their bytes and existing
application rules are unchanged.

| Source CSV | Production consumer |
| --- | --- |
| [commercial_component_overrides.csv](output/commercial_component_overrides.csv) | [select_commercial_measurements.R](../construction_commercial_measurements/code/select_commercial_measurements.R) |
| [commercial_cross_family_decisions.csv](output/commercial_cross_family_decisions.csv) | [select_commercial_measurements.R](../construction_commercial_measurements/code/select_commercial_measurements.R) |
| [commercial_manual_decisions.csv](output/commercial_manual_decisions.csv) | [select_commercial_measurements.R](../construction_commercial_measurements/code/select_commercial_measurements.R) |
| [commercial_reported_land_decisions.csv](output/commercial_reported_land_decisions.csv) | [select_commercial_measurements.R](../construction_commercial_measurements/code/select_commercial_measurements.R) |
| [commercial_reviewed_locations.csv](output/commercial_reviewed_locations.csv) | [calculate_project_boundary_distances.R](../construction_boundary_distances/code/calculate_project_boundary_distances.R) |
| [commercial_verified_case_review.csv](output/commercial_verified_case_review.csv) | [build_preferred_commercial_candidates.R](../construction_commercial_measurements/code/build_preferred_commercial_candidates.R) |
| [corrected_year_zoning_decisions.csv](output/corrected_year_zoning_decisions.csv) | [assemble_construction_data.R](../construction_boundary_distances/code/assemble_construction_data.R) |
| [eligibility_manual_exceptions.csv](output/eligibility_manual_exceptions.csv) | [select_and_correct_residential_measurements.R](../construction_residential_measurements/code/select_and_correct_residential_measurements.R) |
| [historical_address_corrections.csv](output/historical_address_corrections.csv) | [select_historical_coordinates.R](../construction_historical_coordinates/code/select_historical_coordinates.R) |
| [historical_address_matches.csv](output/historical_address_matches.csv) | [match_historical_parcel_records.R](../construction_historical_coordinates/code/match_historical_parcel_records.R) |
| [historical_coordinate_year_corrections.csv](output/historical_coordinate_year_corrections.csv) | [select_historical_coordinates.R](../construction_historical_coordinates/code/select_historical_coordinates.R) |
| [residential_additional_candidate_decisions.csv](output/residential_additional_candidate_decisions.csv) | [assemble_construction_data.R](../construction_boundary_distances/code/assemble_construction_data.R), [select_residential_buildings.R](../construction_residential_measurements/code/select_residential_buildings.R) |
| [residential_class297_component_overrides.csv](output/residential_class297_component_overrides.csv) | [select_and_correct_residential_measurements.R](../construction_residential_measurements/code/select_and_correct_residential_measurements.R) |
| [residential_overlap_decisions.csv](output/residential_overlap_decisions.csv) | [select_and_correct_residential_measurements.R](../construction_residential_measurements/code/select_and_correct_residential_measurements.R), [select_residential_buildings.R](../construction_residential_measurements/code/select_residential_buildings.R) |
| [residential_reviewed_building_components.csv](output/residential_reviewed_building_components.csv) | [build_residential_assessor_projects.R](../construction_residential_identity/code/build_residential_assessor_projects.R) |
| [residential_reviewed_card_selections.csv](output/residential_reviewed_card_selections.csv) | [build_residential_project_candidates.R](../construction_project_candidates/code/build_residential_project_candidates.R) |
| [residential_reviewed_home_replacements.csv](output/residential_reviewed_home_replacements.csv) | [build_residential_assessor_projects.R](../construction_residential_identity/code/build_residential_assessor_projects.R) |
| [residential_reviewed_parcel_locations.csv](output/residential_reviewed_parcel_locations.csv) | [locate_construction_projects.R](../construction_project_locations/code/locate_construction_projects.R) |
| [residential_reviewed_permit_locations.csv](output/residential_reviewed_permit_locations.csv) | [locate_construction_projects.R](../construction_project_locations/code/locate_construction_projects.R) |
| [residential_reviewed_source_duplicates.csv](output/residential_reviewed_source_duplicates.csv) | [build_residential_assessor_projects.R](../construction_residential_identity/code/build_residential_assessor_projects.R) |
| [residential_reviewed_source_exclusions.csv](output/residential_reviewed_source_exclusions.csv) | [build_residential_assessor_projects.R](../construction_residential_identity/code/build_residential_assessor_projects.R) |
| [residential_source_decisions.csv](output/residential_source_decisions.csv) | [select_and_correct_residential_measurements.R](../construction_residential_measurements/code/select_and_correct_residential_measurements.R) |
| [residential_unresolved_source_dispositions.csv](output/residential_unresolved_source_dispositions.csv) | [select_and_correct_residential_measurements.R](../construction_residential_measurements/code/select_and_correct_residential_measurements.R) |

The linked production scripts read these files through Make-managed input links. General Assessor source
selection, duplicate matching, and density calculations remain executable code;
no automatic decision was frozen as a manual exception by this reorganization.

The [audit archive](../audits/construction_review_history/README.md) preserves the
original review tables and research notes. The unchanged historical zoning
files have their own [source task](../construction_zoning_history/README.md).
They are preserved history, not thousands of new manual corrections.

The [construction workflow](../construction_boundary_distances/README.md) shows how ordinary Assessor cleaning, these decisions, density, and boundary distances connect.
