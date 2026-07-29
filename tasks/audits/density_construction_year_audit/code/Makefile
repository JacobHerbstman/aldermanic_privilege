all: ../output/construction_year_model_results.csv

../output/construction_year_history_summary.csv: build_construction_year_history.R ../input/residential_improvement_characteristics_full.csv ../input/residential_cross_section.csv ../input/building_permits_clean.gpkg | ../output
	$(R) $<

../output/construction_year_revision_cases.csv \
../output/construction_year_revision_events.csv \
../output/construction_year_rule_selections.csv \
../output/construction_year_rule_validation.csv \
../output/construction_year_rule_validation_strata.csv \
../output/construction_year_permit_comparison.csv \
../output/construction_year_episode_candidates.csv \
../output/construction_year_review_cases.csv: ../output/construction_year_history_summary.csv
	@test -s "$@"

../output/construction_year_model_results.csv: run_construction_year_models.R ../../../_lib/canonical_geometry_helpers.R ../../../_lib/border_pair_helpers.R ../output/construction_year_rule_selections.csv ../output/construction_year_revision_cases.csv ../input/residential_cross_section.csv ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/multifamily_data_cleaned.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/alderman_uncertainty_index_through2022.csv | ../output
	$(R) $<

../output/construction_year_model_sample_summary.csv \
../output/construction_year_model_changed_rows.csv: ../output/construction_year_model_results.csv
	@test -s "$@"

../input/residential_improvement_characteristics_full.csv: ../../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/residential_cross_section.csv: ../../../residential_improvements_data_cleaning/output/residential_cross_section.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_geometry.gpkg: ../../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/multifamily_data_cleaned.csv: ../../../commercial_value_data_cleaning/output/multifamily_data_cleaned.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_controls.csv: ../../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/residential_improvement_characteristics_full.csv ../input/residential_cross_section.csv ../input/building_permits_clean.gpkg ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/multifamily_data_cleaned.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/alderman_uncertainty_index_through2022.csv

include ../../../shell_functions.make
include ../../../generic.make
