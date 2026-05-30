MAIN_YVARS := 'log(density_far)' 'log(density_dupac)'
MAIN_BW_M ?= 152.4
MAIN_BW_LABEL ?= 500ft
MAIN_FE_SPEC ?= zonegroup_segment_year_additive
MAIN_CLUSTER_LEVEL ?= ward_pair

all: \
	../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL).tex \
	../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL)_pruned.tex

../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL).tex: combine_density_main_table.R ../temp/fe_summary_$(MAIN_BW_LABEL)_all_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL).csv ../temp/fe_summary_$(MAIN_BW_LABEL)_multifamily_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL).csv | ../output
	Rscript combine_density_main_table.R $(MAIN_BW_LABEL) $(MAIN_FE_SPEC) all $(MAIN_CLUSTER_LEVEL)

../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL)_pruned.tex: combine_density_main_table.R ../temp/fe_summary_$(MAIN_BW_LABEL)_all_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL)_pruned.csv ../temp/fe_summary_$(MAIN_BW_LABEL)_multifamily_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL)_pruned.csv | ../output
	Rscript combine_density_main_table.R $(MAIN_BW_LABEL) $(MAIN_FE_SPEC) pruned $(MAIN_CLUSTER_LEVEL)

../temp/fe_summary_$(MAIN_BW_LABEL)_all_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL).csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft Rscript border_pair_FE_regressions.R $(MAIN_BW_M) all $(MAIN_FE_SPEC) all $(MAIN_CLUSTER_LEVEL) $(MAIN_YVARS)

../temp/fe_summary_$(MAIN_BW_LABEL)_multifamily_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL).csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft Rscript border_pair_FE_regressions.R $(MAIN_BW_M) multifamily $(MAIN_FE_SPEC) all $(MAIN_CLUSTER_LEVEL) $(MAIN_YVARS)

../temp/fe_summary_$(MAIN_BW_LABEL)_all_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL)_pruned.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft Rscript border_pair_FE_regressions.R $(MAIN_BW_M) all $(MAIN_FE_SPEC) pruned $(MAIN_CLUSTER_LEVEL) $(MAIN_YVARS)

../temp/fe_summary_$(MAIN_BW_LABEL)_multifamily_$(MAIN_FE_SPEC)_clust_$(MAIN_CLUSTER_LEVEL)_pruned.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft Rscript border_pair_FE_regressions.R $(MAIN_BW_M) multifamily $(MAIN_FE_SPEC) pruned $(MAIN_CLUSTER_LEVEL) $(MAIN_YVARS)

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/confounded_segment_flags.csv: ../../border_confound_pruning/output/confounded_segment_flags.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/parcels_with_ward_distances.csv \
	../input/confounded_pair_era_flags.csv \
	../input/confounded_segment_flags.csv

include ../../generic.make
