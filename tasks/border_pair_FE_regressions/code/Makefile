SHELL := bash

MAIN_YVARS := 'log(density_far)' 'log(density_dupac)'
MAIN_BW_M ?= 152.4
MAIN_BW_LABEL ?= 500ft

all: \
	../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex \
	../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex

../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex: combine_density_main_table.R ../temp/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair.csv ../temp/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv | ../output
	Rscript combine_density_main_table.R ../temp/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair.csv ../temp/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv "$@"

../temp/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(MAIN_BW_LABEL) CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" Rscript border_pair_FE_regressions.R $(MAIN_BW_M) all zonegroup_segment_year_additive ../temp/ignore_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair.tex $(MAIN_YVARS)

../temp/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(MAIN_BW_LABEL) CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" Rscript border_pair_FE_regressions.R $(MAIN_BW_M) multifamily zonegroup_segment_year_additive ../temp/ignore_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex $(MAIN_YVARS)

../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex: combine_density_main_table.R ../temp/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv ../temp/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv | ../output
	Rscript combine_density_main_table.R ../temp/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv ../temp/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv "$@"

../temp/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(MAIN_BW_LABEL) PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" Rscript border_pair_FE_regressions.R $(MAIN_BW_M) all zonegroup_segment_year_additive ../temp/ignore_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex $(MAIN_YVARS)

../temp/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(MAIN_BW_LABEL) PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" Rscript border_pair_FE_regressions.R $(MAIN_BW_M) multifamily zonegroup_segment_year_additive ../temp/ignore_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex $(MAIN_YVARS)

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/confounded_segment_flags.csv: ../../border_confound_pruning/output/confounded_segment_flags.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv

include ../../shell_functions.make
include ../../generic.make
