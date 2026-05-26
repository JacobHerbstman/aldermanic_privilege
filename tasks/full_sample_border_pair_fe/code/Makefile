SHELL := bash

all: \
	../output/fe_model_summary_bwall_all_zonegroup_segment_year_additive_clust_segment.csv \
	../output/fe_table_bwall_all_zonegroup_segment_year_additive_clust_segment.tex \
	../output/fe_model_summary_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.csv \
	../output/fe_table_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.tex

../output/fe_model_summary_bwall_all_zonegroup_segment_year_additive_clust_segment.csv: ../../_lib/border_pair_fe_regression_runner.R ../input/parcels_with_ward_distances_canonical.csv | ../output
	FE_INPUT_PATH=../input/parcels_with_ward_distances_canonical.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_model_summary_bwall_all_zonegroup_segment_year_additive_clust_segment.csv CLUSTER_LEVEL=segment Rscript ../../_lib/border_pair_fe_regression_runner.R all all zonegroup_segment_year_additive ../output/fe_table_bwall_all_zonegroup_segment_year_additive_clust_segment.tex 'log(density_far)' 'log(density_dupac)'

../output/fe_table_bwall_all_zonegroup_segment_year_additive_clust_segment.tex: ../output/fe_model_summary_bwall_all_zonegroup_segment_year_additive_clust_segment.csv
	@test -s "$@"

../output/fe_model_summary_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.csv: ../../_lib/border_pair_fe_regression_runner.R ../input/parcels_with_ward_distances_canonical.csv | ../output
	FE_INPUT_PATH=../input/parcels_with_ward_distances_canonical.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_model_summary_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.csv CLUSTER_LEVEL=segment Rscript ../../_lib/border_pair_fe_regression_runner.R all multifamily zonegroup_segment_year_additive ../output/fe_table_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.tex 'log(density_far)' 'log(density_dupac)'

../output/fe_table_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.tex: ../output/fe_model_summary_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.csv
	@test -s "$@"

../input/parcels_with_ward_distances_canonical.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances_canonical.csv

include ../../shell_functions.make
include ../../generic.make
