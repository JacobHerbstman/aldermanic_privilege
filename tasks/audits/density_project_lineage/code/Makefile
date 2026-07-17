EXACT_MATCH_FT ?= 10
LOCAL_MATCH_FT ?= 150
MAX_BUILDING_GAP ?= 0.10

all: ../output/density_project_lineage.csv ../output/density_project_lineage_summary.csv ../output/density_project_lineage_unresolved_500ft.csv ../output/density_project_lineage_models.csv ../output/density_project_lineage_rd_4panel.png ../output/density_project_lineage_side_density_summary.csv ../output/density_historical_parcel_coverage.csv ../output/density_parcel_address_audit_summary.csv ../output/density_parcel_address_recovered_boundaries.csv ../output/density_bandwidth_fe_robustness.csv ../output/density_all_continuous_ward_pair_influence.csv ../output/density_final_regression_table_preview.tex ../output/density_lineage_zoning_models.csv

../output/density_project_lineage.csv: build_density_project_lineage.R ../input/density_historical_coordinate_cases.csv ../input/density_historical_parcel_records.csv ../input/density_historical_recovery_membership.csv ../input/density_year_built_revision_cases.csv ../input/residential_cross_section.csv ../input/multifamily_data_cleaned.csv ../input/parcel_universe_2025_city.csv ../input/parcels_with_geometry.gpkg | ../output
	Rscript $< $(EXACT_MATCH_FT) $(LOCAL_MATCH_FT) $(MAX_BUILDING_GAP)

../output/density_project_lineage_summary.csv: ../output/density_project_lineage.csv

../output/density_project_lineage_unresolved_500ft.csv: ../output/density_project_lineage.csv

../output/density_project_lineage_models.csv: estimate_density_project_lineage.R ../../../_lib/border_pair_helpers.R ../output/density_project_lineage.csv ../output/density_parcel_address_recovered_model_rows.csv ../input/density_historical_recovered_exact_rows.csv ../input/parcels_with_ward_distances.csv | ../output
	Rscript $<

../output/density_project_lineage_rd_4panel.png: plot_density_project_lineage.R ../../../_lib/border_pair_helpers.R ../output/density_project_lineage.csv ../output/density_parcel_address_lineage_evidence.csv ../output/density_parcel_address_recovered_model_rows.csv ../output/density_bandwidth_fe_robustness.csv ../input/density_historical_recovered_exact_rows.csv ../input/parcels_with_ward_distances.csv | ../output
	Rscript $<

../output/density_project_lineage_rd_4panel.pdf \
../output/density_project_lineage_rd_all_far.png \
../output/density_project_lineage_rd_multifamily_far.png \
../output/density_project_lineage_rd_all_dupac.png \
../output/density_project_lineage_rd_multifamily_dupac.png: ../output/density_project_lineage_rd_4panel.png
	@test -s "$@"

../output/density_project_lineage_side_density_summary.csv: diagnose_density_project_lineage.R ../../../_lib/border_pair_helpers.R ../output/density_project_lineage.csv ../output/density_project_lineage_models.csv ../input/density_historical_recovered_exact_rows.csv ../input/parcels_with_ward_distances.csv ../input/parcel_universe_2025_city.csv | ../output
	Rscript $<

../output/density_project_lineage_condo_summary.csv \
../output/density_project_lineage_ward_pair_density.csv \
../output/density_project_lineage_ward_pair_influence.csv \
../output/density_project_lineage_ward_pair_group_influence.csv \
../output/density_project_lineage_diagnostics.png: ../output/density_project_lineage_side_density_summary.csv
	@test -s "$@"

../output/density_historical_building_universe.csv: prepare_historical_parcel_universe_audit.R ../input/residential_cross_section.csv ../input/multifamily_data_cleaned.csv ../input/parcel_universe_2025_city.csv | ../output
	Rscript $<

../output/density_historical_exact_parcel_records.csv: fetch_historical_parcel_universe_exact.R ../output/density_historical_building_universe.csv | ../output
	Rscript $<

../output/density_historical_parcel_coverage.csv: summarize_historical_parcel_universe_audit.R ../output/density_historical_building_universe.csv ../output/density_historical_exact_parcel_records.csv ../input/parcel_universe_2025_city.csv | ../output
	Rscript $<

../output/density_historical_geocoded_buildings.csv \
../output/density_historical_selected_geocoded_buildings.csv \
../output/density_historical_parcel_coverage_by_current_status.csv \
../output/density_historical_parcel_coverage_by_year.csv \
../output/density_historical_parcel_unmatched.csv: ../output/density_historical_parcel_coverage.csv
	@test -s "$@"

../output/density_parcel_address_targets.csv: prepare_parcel_address_audit_targets.R ../output/density_project_lineage.csv ../output/density_historical_parcel_unmatched.csv ../input/density_historical_recovered_exact_rows.csv | ../output
	Rscript $<

../output/density_historical_address_records.csv: fetch_historical_parcel_addresses.R ../output/density_parcel_address_targets.csv | ../output
	Rscript $<

../output/density_parcel_address_audit_summary.csv: audit_parcel_address_recovery.R ../output/density_parcel_address_targets.csv ../output/density_historical_address_records.csv ../output/density_project_lineage.csv ../output/density_historical_parcel_unmatched.csv ../output/density_historical_building_universe.csv ../input/density_historical_recovered_exact_rows.csv ../input/density_historical_parcel_records.csv ../input/parcel_addresses_2025_chicago.csv ../input/parcel_universe_2025_city.csv | ../output
	Rscript $<

../output/density_parcel_address_selected_history.csv \
../output/density_parcel_address_unlocated_recovery.csv \
../output/density_parcel_address_lineage_evidence.csv \
../output/density_parcel_address_lineage_summary.csv: ../output/density_parcel_address_audit_summary.csv
	@test -s "$@"

../output/density_parcel_address_recovered_boundaries.csv: assign_address_recovered_boundaries.R ../../../_lib/canonical_geometry_helpers.R ../output/density_parcel_address_unlocated_recovery.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg | ../output
	Rscript $<

../output/density_parcel_address_recovered_model_rows.csv: build_address_recovered_model_rows.R ../../../_lib/canonical_geometry_helpers.R ../output/density_parcel_address_unlocated_recovery.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/zoning_data_clean.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls_2000_2023.csv ../input/alderman_uncertainty_index_through2022.csv | ../output
	Rscript $<

../output/density_bandwidth_fe_robustness.csv: estimate_density_bandwidth_fe_robustness.R ../../../_lib/border_pair_helpers.R ../output/density_project_lineage.csv ../output/density_parcel_address_lineage_evidence.csv ../output/density_parcel_address_recovered_model_rows.csv ../input/density_historical_recovered_exact_rows.csv ../input/parcels_with_ward_distances.csv | ../output
	Rscript $<

../output/density_all_continuous_ward_pair_influence.csv: audit_density_positive_pair_influence.R ../../../_lib/border_pair_helpers.R ../output/density_project_lineage.csv ../output/density_parcel_address_lineage_evidence.csv ../output/density_parcel_address_recovered_model_rows.csv ../input/density_historical_recovered_exact_rows.csv ../input/parcels_with_ward_distances.csv | ../output
	Rscript $<

../output/density_final_regression_table_preview.tex: build_density_final_table_preview.R ../output/density_bandwidth_fe_robustness.csv | ../output
	Rscript $<

../output/density_lineage_zoning_models.csv: audit_density_lineage_zoning.R ../../../_lib/border_pair_helpers.R ../output/density_project_lineage.csv ../output/density_parcel_address_lineage_evidence.csv ../output/density_parcel_address_recovered_model_rows.csv ../input/density_historical_recovered_exact_rows.csv ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg ../input/zoning_data_clean.gpkg | ../output
	Rscript $<

../output/density_lineage_zoning_attrition.csv \
../output/density_lineage_zoning_excluded_rows.csv \
../output/density_lineage_zoning_transitions.csv: ../output/density_lineage_zoning_models.csv
	@test -s "$@"

../input/density_historical_coordinate_cases.csv: ../../paper_code_alignment_audit/output/density_historical_coordinate_cases.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_historical_parcel_records.csv: ../../paper_code_alignment_audit/output/density_historical_parcel_records.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_historical_recovery_membership.csv: ../../paper_code_alignment_audit/output/density_historical_recovery_membership.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_year_built_revision_cases.csv: ../../paper_code_alignment_audit/output/density_year_built_revision_cases.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/residential_cross_section.csv: ../../../residential_improvements_data_cleaning/output/residential_cross_section.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/multifamily_data_cleaned.csv: ../../../commercial_value_data_cleaning/output/multifamily_data_cleaned.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_universe_2025_city.csv: ../../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_geometry.gpkg: ../../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_historical_recovered_exact_rows.csv: ../../paper_code_alignment_audit/output/density_historical_recovered_exact_rows.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_addresses_2025_chicago.csv: ../../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/zoning_data_clean.gpkg: ../../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_controls_2000_2023.csv: ../../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/density_historical_coordinate_cases.csv ../input/density_historical_parcel_records.csv ../input/density_historical_recovery_membership.csv ../input/density_year_built_revision_cases.csv ../input/residential_cross_section.csv ../input/multifamily_data_cleaned.csv ../input/parcel_universe_2025_city.csv ../input/parcels_with_geometry.gpkg ../input/density_historical_recovered_exact_rows.csv ../input/parcels_with_ward_distances.csv ../input/parcel_addresses_2025_chicago.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/zoning_data_clean.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls_2000_2023.csv ../input/alderman_uncertainty_index_through2022.csv

include ../../../generic.make
