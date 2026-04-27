SHELL := bash
include ../../../shell_functions.make

all: ../output/source_attrition_ladder.csv \
	../output/census_bps_chicago_benchmark.csv \
	../output/outcome_vs_bps_yearly_gap.csv \
	../output/commercial_large_residential_review.csv \
	../output/commercial_large_gap_closure_scenarios.csv \
	../output/permit_large_residential_review.csv \
	../output/permit_large_assessor_trace.csv \
	../output/raw_candidate_to_final_crosswalk.csv \
	../output/possible_missed_new_residential.csv \
	../output/manual_google_review_queue.csv \
	../output/new_construction_completeness_audit_report.md

../output/source_attrition_ladder.csv: run_new_construction_completeness_audit.R ../../../_lib/canonical_geometry_helpers.R ../input/residential_improvement_characteristics.csv ../input/residential_improvements_panel.parquet ../input/commercial_value_raw.csv ../input/building_permits_raw.csv ../input/parcel_universe.csv ../input/residential_cross_section.csv ../input/multifamily_data_cleaned.csv ../input/parcels_pre_scores.csv ../input/parcels_with_ward_distances.csv ../input/parcel_segment_ids.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg | ../output
	$(R) run_new_construction_completeness_audit.R

../output/raw_candidate_to_final_crosswalk.csv \
../output/census_bps_chicago_benchmark.csv \
../output/outcome_vs_bps_yearly_gap.csv \
../output/commercial_large_residential_review.csv \
../output/commercial_large_gap_closure_scenarios.csv \
../output/permit_large_residential_review.csv \
../output/permit_large_assessor_trace.csv \
../output/possible_missed_new_residential.csv \
../output/manual_google_review_queue.csv \
../output/new_construction_completeness_audit_report.md: ../output/source_attrition_ladder.csv
	test -f "$@"

../input/residential_improvement_characteristics.csv: ../../../../data_raw/Assessor_-_Single_and_Multi-Family_Improvement_Characteristics_20251111.csv | ../input
	ln -sf "$<" "$@"

../input/residential_improvements_panel.parquet: ../../../process_residential_improvements_full/output/residential_improvements_panel.parquet | ../input
	ln -sf "$<" "$@"

../input/commercial_value_raw.csv: ../../../../data_raw/Assessor_-_Commercial_Valuation_Data_20251120.csv | ../input
	ln -sf "$<" "$@"

../input/building_permits_raw.csv: ../../../../data_raw/Building_Permits_20251121.csv | ../input
	ln -sf "$<" "$@"

../input/parcel_universe.csv: ../../../../data_raw/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv | ../input
	ln -sf "$<" "$@"

../input/residential_cross_section.csv: ../../../residential_improvements_data_cleaning/output/residential_cross_section.csv | ../input
	ln -sf "$<" "$@"

../input/multifamily_data_cleaned.csv: ../../../commercial_value_data_cleaning/output/multifamily_data_cleaned.csv | ../input
	ln -sf "$<" "$@"

../input/parcels_pre_scores.csv: ../../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/residential_improvement_characteristics.csv ../input/residential_improvements_panel.parquet ../input/commercial_value_raw.csv ../input/building_permits_raw.csv ../input/parcel_universe.csv ../input/residential_cross_section.csv ../input/multifamily_data_cleaned.csv ../input/parcels_pre_scores.csv ../input/parcels_with_ward_distances.csv ../input/parcel_segment_ids.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg

include ../../../generic.make
