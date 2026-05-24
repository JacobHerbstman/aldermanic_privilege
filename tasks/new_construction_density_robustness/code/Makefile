SHELL := bash

DENSITY_ROBUSTNESS_BW_M ?= 152.4
DENSITY_ROBUSTNESS_BW_LABEL ?= 500ft

all: ../output/fe_table_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_mainoutcomes.tex

../output/fe_table_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_mainoutcomes.tex: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_low_discretion_residualized.csv | ../output
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(DENSITY_ROBUSTNESS_BW_LABEL) CLUSTER_LEVEL=ward_pair FE_INPUT_PATH=../output/parcels_with_ward_distances_low_discretion_residualized.csv Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R $(DENSITY_ROBUSTNESS_BW_M) all zonegroup_segment_year_additive "$@" "log(density_far)" "log(density_dupac)"

../output/parcels_with_ward_distances_low_discretion_residualized.csv: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/alderman_uncertainty_index_low_discretion_residualized.csv | ../output
	PARCELS_INPUT_PATH=../input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances_low_discretion_residualized.csv MERGE_SUMMARY_OUTPUT_PATH=../output/boundary_distance_summary_low_discretion_residualized.csv SCORE_COVERAGE_OUTPUT_PATH=../output/score_merge_coverage_low_discretion_residualized.csv Rscript ../../merge_in_scores/code/merge_scores.R ../input/alderman_uncertainty_index_low_discretion_residualized.csv uncertainty_index

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_low_discretion_residualized.csv: ../../new_construction_score_variants/output/alderman_uncertainty_index_low_discretion_residualized.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/alderman_uncertainty_index_low_discretion_residualized.csv

include ../../shell_functions.make
include ../../generic.make
