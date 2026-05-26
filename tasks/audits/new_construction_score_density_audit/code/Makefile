SHELL := bash

BASELINE_SPEC := ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022
DENSITY_ROBUSTNESS_BW_M ?= 152.4
DENSITY_ROBUSTNESS_BW_LABEL ?= 500ft
MAX_PERMIT_YEAR ?= 2022

all: \
	../output/new_construction_processing_time_summary.csv \
	../output/new_construction_demolition_processing_time_summary.csv \
	../output/renovation_text_classification_summary_restricted_renovation.csv \
	../output/boundary_distance_summary_baseline.csv \
	../output/boundary_distance_summary_new_construction.csv \
	../output/boundary_distance_summary_new_construction_demolition.csv \
	../output/boundary_distance_summary_restricted_renovation.csv \
	../output/boundary_distance_summary_low_discretion_residualized.csv \
	../output/density_score_robustness_comparison.csv \
	../output/density_score_robustness_comparison_new_construction_demolition.csv \
	../output/density_score_robustness_comparison_restricted_renovation.csv \
	../output/density_score_robustness_comparison_low_discretion_residualized.csv

../output/new_construction_processing_time_summary.csv: summarize_new_construction_processing_time.R ../../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript $< ../input/permits_for_uncertainty_index.csv ../output/new_construction_processing_time_summary.csv ../output/new_construction_processing_time_summary.tex ../output/new_construction_alderman_processing_times.csv ../output/new_construction_alderman_processing_time_density.pdf new_construction "New construction only" "new_construction" $(MAX_PERMIT_YEAR)

../output/new_construction_demolition_processing_time_summary.csv: summarize_new_construction_processing_time.R ../../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript $< ../input/permits_for_uncertainty_index.csv ../output/new_construction_demolition_processing_time_summary.csv ../output/new_construction_demolition_processing_time_summary.tex ../output/new_construction_demolition_alderman_processing_times.csv ../output/new_construction_demolition_alderman_processing_time_density.pdf new_construction_demolition "New construction + demolition" "new_construction,demolition" $(MAX_PERMIT_YEAR)

../output/renovation_text_classification_summary_restricted_renovation.csv: classify_restricted_renovation_text.R ../../../_lib/alderman_uncertainty_helpers.R ../../../_lib/restricted_renovation_classification.R ../input/permits_for_uncertainty_index.csv ../input/building_permits_clean.gpkg | ../output
	Rscript $< ../input/permits_for_uncertainty_index.csv ../input/building_permits_clean.gpkg ../output/renovation_text_classification_restricted_renovation.csv ../output/renovation_text_classification_summary_restricted_renovation.csv ../output/renovation_text_classification_porch_keyword_summary_restricted_renovation.csv ../output/renovation_text_classification_mixed_job_summary_restricted_renovation.csv ../output/renovation_text_classification_dropped_description_review_restricted_renovation.csv ../output/permits_for_uncertainty_index_restricted_renovation.csv $(MAX_PERMIT_YEAR)

../output/parcels_with_ward_distances_baseline.csv: ../../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../output
	cd ../../../merge_in_scores/code && PARCELS_INPUT_PATH=../../audits/new_construction_score_density_audit/input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../../audits/new_construction_score_density_audit/input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/parcels_with_ward_distances_baseline.csv MERGE_SUMMARY_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/boundary_distance_summary_baseline.csv SCORE_COVERAGE_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/score_merge_coverage_baseline.csv Rscript merge_scores.R ../../audits/new_construction_score_density_audit/input/alderman_uncertainty_index_$(BASELINE_SPEC).csv uncertainty_index

../output/boundary_distance_summary_baseline.csv: ../output/parcels_with_ward_distances_baseline.csv
	@test -s "$@"

../output/parcels_with_ward_distances_new_construction.csv: ../../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/alderman_uncertainty_index_new_construction.csv | ../output
	cd ../../../merge_in_scores/code && PARCELS_INPUT_PATH=../../audits/new_construction_score_density_audit/input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../../audits/new_construction_score_density_audit/input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/parcels_with_ward_distances_new_construction.csv MERGE_SUMMARY_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/boundary_distance_summary_new_construction.csv SCORE_COVERAGE_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/score_merge_coverage_new_construction.csv Rscript merge_scores.R ../../audits/new_construction_score_density_audit/input/alderman_uncertainty_index_new_construction.csv uncertainty_index

../output/boundary_distance_summary_new_construction.csv: ../output/parcels_with_ward_distances_new_construction.csv
	@test -s "$@"

../output/parcels_with_ward_distances_new_construction_demolition.csv: ../../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/alderman_uncertainty_index_new_construction_demolition.csv | ../output
	cd ../../../merge_in_scores/code && PARCELS_INPUT_PATH=../../audits/new_construction_score_density_audit/input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../../audits/new_construction_score_density_audit/input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/parcels_with_ward_distances_new_construction_demolition.csv MERGE_SUMMARY_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/boundary_distance_summary_new_construction_demolition.csv SCORE_COVERAGE_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/score_merge_coverage_new_construction_demolition.csv Rscript merge_scores.R ../../audits/new_construction_score_density_audit/input/alderman_uncertainty_index_new_construction_demolition.csv uncertainty_index

../output/boundary_distance_summary_new_construction_demolition.csv: ../output/parcels_with_ward_distances_new_construction_demolition.csv
	@test -s "$@"

../output/parcels_with_ward_distances_restricted_renovation.csv: ../../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/alderman_uncertainty_index_restricted_renovation.csv | ../output
	cd ../../../merge_in_scores/code && PARCELS_INPUT_PATH=../../audits/new_construction_score_density_audit/input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../../audits/new_construction_score_density_audit/input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/parcels_with_ward_distances_restricted_renovation.csv MERGE_SUMMARY_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/boundary_distance_summary_restricted_renovation.csv SCORE_COVERAGE_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/score_merge_coverage_restricted_renovation.csv Rscript merge_scores.R ../../audits/new_construction_score_density_audit/input/alderman_uncertainty_index_restricted_renovation.csv uncertainty_index

../output/boundary_distance_summary_restricted_renovation.csv: ../output/parcels_with_ward_distances_restricted_renovation.csv
	@test -s "$@"

../output/parcels_with_ward_distances_low_discretion_residualized.csv: ../../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/alderman_uncertainty_index_low_discretion_residualized.csv | ../output
	cd ../../../merge_in_scores/code && PARCELS_INPUT_PATH=../../audits/new_construction_score_density_audit/input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../../audits/new_construction_score_density_audit/input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/parcels_with_ward_distances_low_discretion_residualized.csv MERGE_SUMMARY_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/boundary_distance_summary_low_discretion_residualized.csv SCORE_COVERAGE_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/score_merge_coverage_low_discretion_residualized.csv Rscript merge_scores.R ../../audits/new_construction_score_density_audit/input/alderman_uncertainty_index_low_discretion_residualized.csv uncertainty_index

../output/boundary_distance_summary_low_discretion_residualized.csv: ../output/parcels_with_ward_distances_low_discretion_residualized.csv
	@test -s "$@"

../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv: ../../../_lib/border_pair_fe_regression_runner.R ../output/parcels_with_ward_distances_baseline.csv | ../output
	cd ../../../border_pair_FE_regressions/code && FE_INPUT_PATH=../../audits/new_construction_score_density_audit/output/parcels_with_ward_distances_baseline.csv FE_SUMMARY_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv CLUSTER_LEVEL=ward_pair Rscript ../../_lib/border_pair_fe_regression_runner.R $(DENSITY_ROBUSTNESS_BW_M) all zonegroup_segment_year_additive ../../audits/new_construction_score_density_audit/output/fe_table_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"

../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_new_construction.csv: ../../../_lib/border_pair_fe_regression_runner.R ../output/parcels_with_ward_distances_new_construction.csv | ../output
	cd ../../../border_pair_FE_regressions/code && FE_INPUT_PATH=../../audits/new_construction_score_density_audit/output/parcels_with_ward_distances_new_construction.csv FE_SUMMARY_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_new_construction.csv CLUSTER_LEVEL=ward_pair Rscript ../../_lib/border_pair_fe_regression_runner.R $(DENSITY_ROBUSTNESS_BW_M) all zonegroup_segment_year_additive ../../audits/new_construction_score_density_audit/output/fe_table_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_new_construction.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"

../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.csv: ../../../_lib/border_pair_fe_regression_runner.R ../output/parcels_with_ward_distances_new_construction_demolition.csv | ../output
	cd ../../../border_pair_FE_regressions/code && FE_INPUT_PATH=../../audits/new_construction_score_density_audit/output/parcels_with_ward_distances_new_construction_demolition.csv FE_SUMMARY_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.csv CLUSTER_LEVEL=ward_pair Rscript ../../_lib/border_pair_fe_regression_runner.R $(DENSITY_ROBUSTNESS_BW_M) all zonegroup_segment_year_additive ../../audits/new_construction_score_density_audit/output/fe_table_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"

../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.csv: ../../../_lib/border_pair_fe_regression_runner.R ../output/parcels_with_ward_distances_restricted_renovation.csv | ../output
	cd ../../../border_pair_FE_regressions/code && FE_INPUT_PATH=../../audits/new_construction_score_density_audit/output/parcels_with_ward_distances_restricted_renovation.csv FE_SUMMARY_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.csv CLUSTER_LEVEL=ward_pair Rscript ../../_lib/border_pair_fe_regression_runner.R $(DENSITY_ROBUSTNESS_BW_M) all zonegroup_segment_year_additive ../../audits/new_construction_score_density_audit/output/fe_table_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"

../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.csv: ../../../_lib/border_pair_fe_regression_runner.R ../output/parcels_with_ward_distances_low_discretion_residualized.csv | ../output
	cd ../../../border_pair_FE_regressions/code && FE_INPUT_PATH=../../audits/new_construction_score_density_audit/output/parcels_with_ward_distances_low_discretion_residualized.csv FE_SUMMARY_OUTPUT_PATH=../../audits/new_construction_score_density_audit/output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.csv CLUSTER_LEVEL=ward_pair Rscript ../../_lib/border_pair_fe_regression_runner.R $(DENSITY_ROBUSTNESS_BW_M) all zonegroup_segment_year_additive ../../audits/new_construction_score_density_audit/output/fe_table_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"

../output/density_score_robustness_comparison.csv: summarize_new_construction_density_robustness.R ../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_new_construction.csv ../output/parcels_with_ward_distances_baseline.csv ../output/parcels_with_ward_distances_new_construction.csv | ../output
	DENSITY_ROBUSTNESS_BW_M=$(DENSITY_ROBUSTNESS_BW_M) DENSITY_ROBUSTNESS_BW_LABEL=$(DENSITY_ROBUSTNESS_BW_LABEL) DENSITY_ROBUSTNESS_SAMPLE=all DENSITY_ROBUSTNESS_CLUSTER=ward_pair Rscript $< ../output

../output/density_score_robustness_comparison_new_construction_demolition.csv: summarize_new_construction_density_robustness.R ../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.csv ../output/parcels_with_ward_distances_baseline.csv ../output/parcels_with_ward_distances_new_construction_demolition.csv | ../output
	DENSITY_ROBUSTNESS_BW_M=$(DENSITY_ROBUSTNESS_BW_M) DENSITY_ROBUSTNESS_BW_LABEL=$(DENSITY_ROBUSTNESS_BW_LABEL) DENSITY_ROBUSTNESS_SAMPLE=all DENSITY_ROBUSTNESS_CLUSTER=ward_pair Rscript $< ../output new_construction_demolition "New construction + demolition" ../output/density_score_robustness_comparison_new_construction_demolition.csv ../output/density_score_robustness_comparison_new_construction_demolition.tex ../output/parcel_sign_agreement_baseline_vs_new_construction_demolition.csv

../output/density_score_robustness_comparison_restricted_renovation.csv: summarize_new_construction_density_robustness.R ../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.csv ../output/parcels_with_ward_distances_baseline.csv ../output/parcels_with_ward_distances_restricted_renovation.csv | ../output
	DENSITY_ROBUSTNESS_BW_M=$(DENSITY_ROBUSTNESS_BW_M) DENSITY_ROBUSTNESS_BW_LABEL=$(DENSITY_ROBUSTNESS_BW_LABEL) DENSITY_ROBUSTNESS_SAMPLE=all DENSITY_ROBUSTNESS_CLUSTER=ward_pair Rscript $< ../output restricted_renovation "Restricted renovation" ../output/density_score_robustness_comparison_restricted_renovation.csv ../output/density_score_robustness_comparison_restricted_renovation.tex ../output/parcel_sign_agreement_baseline_vs_restricted_renovation.csv

../output/density_score_robustness_comparison_low_discretion_residualized.csv: summarize_new_construction_density_robustness.R ../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../output/fe_summary_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.csv ../output/parcels_with_ward_distances_baseline.csv ../output/parcels_with_ward_distances_low_discretion_residualized.csv | ../output
	DENSITY_ROBUSTNESS_BW_M=$(DENSITY_ROBUSTNESS_BW_M) DENSITY_ROBUSTNESS_BW_LABEL=$(DENSITY_ROBUSTNESS_BW_LABEL) DENSITY_ROBUSTNESS_SAMPLE=all DENSITY_ROBUSTNESS_CLUSTER=ward_pair Rscript $< ../output low_discretion_residualized "Residualized low-discretion score" ../output/density_score_robustness_comparison_low_discretion_residualized.csv ../output/density_score_robustness_comparison_low_discretion_residualized.tex ../output/parcel_sign_agreement_baseline_vs_low_discretion_residualized.csv

../input/permits_for_uncertainty_index.csv: ../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_pre_scores.csv: ../../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_new_construction.csv: ../../../new_construction_score_variants/output/alderman_uncertainty_index_new_construction.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_new_construction_demolition.csv: ../../../new_construction_score_variants/output/alderman_uncertainty_index_new_construction_demolition.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_restricted_renovation.csv: ../../../new_construction_score_variants/output/alderman_uncertainty_index_restricted_renovation.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_low_discretion_residualized.csv: ../../../new_construction_score_variants/output/alderman_uncertainty_index_low_discretion_residualized.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv ../input/building_permits_clean.gpkg ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/alderman_uncertainty_index_new_construction.csv ../input/alderman_uncertainty_index_new_construction_demolition.csv ../input/alderman_uncertainty_index_restricted_renovation.csv ../input/alderman_uncertainty_index_low_discretion_residualized.csv

include ../../../shell_functions.make
include ../../../generic.make
