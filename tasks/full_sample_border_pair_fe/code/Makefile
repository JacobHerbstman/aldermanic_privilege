SHELL := bash
include ../../shell_functions.make

all: \
	../output/fe_model_summary_bwall_all_zonegroup_segment_year_additive_clust_segment.csv \
	../output/fe_table_bwall_all_zonegroup_segment_year_additive_clust_segment.tex \
	../output/fe_model_summary_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.csv \
	../output/fe_table_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.tex

../output/fe_model_summary_bwall_all_zonegroup_segment_year_additive_clust_segment.csv: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../input/parcels_with_ward_distances_canonical.csv | ../output
	FE_INPUT_PATH=../input/parcels_with_ward_distances_canonical.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_model_summary_bwall_all_zonegroup_segment_year_additive_clust_segment.csv CLUSTER_LEVEL=segment Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R all all zonegroup_segment_year_additive ../output/fe_table_bwall_all_zonegroup_segment_year_additive_clust_segment.tex 'log(density_far)' 'log(density_dupac)'

../output/fe_table_bwall_all_zonegroup_segment_year_additive_clust_segment.tex: ../output/fe_model_summary_bwall_all_zonegroup_segment_year_additive_clust_segment.csv
	test -f "$@"

../output/fe_model_summary_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.csv: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../input/parcels_with_ward_distances_canonical.csv | ../output
	FE_INPUT_PATH=../input/parcels_with_ward_distances_canonical.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_model_summary_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.csv CLUSTER_LEVEL=segment Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R all multifamily zonegroup_segment_year_additive ../output/fe_table_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.tex 'log(density_far)' 'log(density_dupac)'

../output/fe_table_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.tex: ../output/fe_model_summary_bwall_multifamily_zonegroup_segment_year_additive_clust_segment.csv
	test -f "$@"

../input/parcels_with_ward_distances_canonical.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances_canonical.csv

include ../../generic.make
