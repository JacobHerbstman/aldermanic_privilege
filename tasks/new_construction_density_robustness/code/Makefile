SHELL := bash

DENSITY_ROBUSTNESS_BW_M ?= 152.4
DENSITY_ROBUSTNESS_BW_LABEL ?= 500ft

all: ../output/fe_table_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_mainoutcomes.tex

../output/fe_table_$(DENSITY_ROBUSTNESS_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_mainoutcomes.tex: ../../_lib/border_pair_fe_regression_runner.R ../temp/parcels_with_ward_distances_low_discretion_residualized.csv | ../output
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(DENSITY_ROBUSTNESS_BW_LABEL) CLUSTER_LEVEL=ward_pair FE_INPUT_TAG=low_discretion_residualized FE_TABLE_TAG=low_discretion_residualized_mainoutcomes WRITE_TEX=TRUE Rscript ../../_lib/border_pair_fe_regression_runner.R $(DENSITY_ROBUSTNESS_BW_M) all zonegroup_segment_year_additive all ward_pair "log(density_far)" "log(density_dupac)"

../temp/parcels_with_ward_distances_low_discretion_residualized.csv: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/aldermen_uncertainty_scores.csv | ../temp
	MERGE_OUTPUT_TAG=low_discretion_residualized Rscript ../../merge_in_scores/code/merge_scores.R uncertainty_index

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores.csv: ../../new_construction_score_variants/output/alderman_uncertainty_index_low_discretion_residualized.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/aldermen_uncertainty_scores.csv

include ../../generic.make
