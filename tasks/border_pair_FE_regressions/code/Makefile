SHELL := bash
include ../../shell_functions.make

MAIN_YVARS := 'log(density_far)' 'log(density_dupac)'

all: \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex \
	../output/fe_table_bw500_multifamily_zonegroup_pair_year_additive_clust_ward_pair.tex \
	../output/fe_table_bw250_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive "$@" $(MAIN_YVARS)

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive "$@" $(MAIN_YVARS)

../output/fe_table_bw500_multifamily_zonegroup_pair_year_additive_clust_ward_pair.tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R 500 multifamily zonegroup_pair_year_additive "$@" $(MAIN_YVARS)

../output/fe_table_bw250_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R 250 multifamily zonegroup_segment_year_additive "$@" $(MAIN_YVARS)

../output/fe_table_bw1000_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R 1000 multifamily zonegroup_segment_year_additive "$@" $(MAIN_YVARS)

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv

include ../../generic.make
