SHELL := bash
include ../../shell_functions.make

MAIN_YVARS := 'log(density_far)' 'log(density_dupac)'
UNITS_LOTSIZE_YVARS := 'log(density_far)' 'log(density_dupac)' 'log(unitscount)' 'log(arealotsf)'
MAIN_BW_M ?= 100
LEGACY_NARROW_BW_FT ?= 250
LEGACY_MAIN_BW_FT ?= 500
LEGACY_WIDE_BW_FT ?= 1000

all: \
	../output/fe_table_$(MAIN_BW_M)m_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex \
	../output/fe_table_$(MAIN_BW_M)m_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex \
	../output/fe_summary_$(MAIN_BW_M)m_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv \
	../output/fe_table_bw$(LEGACY_NARROW_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair.tex \
	../output/fe_table_bw$(LEGACY_NARROW_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex \
	../output/fe_summary_bw$(LEGACY_NARROW_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair.csv \
	../output/fe_summary_bw$(LEGACY_NARROW_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv \
	../output/fe_summary_bw$(LEGACY_NARROW_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv \
	../output/fe_table_bw$(LEGACY_MAIN_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex \
	../output/fe_table_bw$(LEGACY_MAIN_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex \
	../output/fe_summary_bw$(LEGACY_MAIN_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv \
	../output/fe_table_bw$(LEGACY_MAIN_BW_FT)_multifamily_zonegroup_pair_year_additive_clust_ward_pair.tex \
	../output/fe_table_bw$(LEGACY_NARROW_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex

../output/fe_table_$(MAIN_BW_M)m_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex: combine_density_main_100m_table.R ../output/fe_summary_$(MAIN_BW_M)m_all_zonegroup_segment_year_additive_clust_ward_pair.csv ../output/fe_summary_$(MAIN_BW_M)m_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv | ../output
	Rscript combine_density_main_100m_table.R ../output/fe_summary_$(MAIN_BW_M)m_all_zonegroup_segment_year_additive_clust_ward_pair.csv ../output/fe_summary_$(MAIN_BW_M)m_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv "$@"

../output/fe_summary_$(MAIN_BW_M)m_all_zonegroup_segment_year_additive_clust_ward_pair.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" Rscript border_pair_FE_regressions.R $(MAIN_BW_M) all zonegroup_segment_year_additive ../output/fe_table_$(MAIN_BW_M)m_all_zonegroup_segment_year_additive_clust_ward_pair.tex $(MAIN_YVARS)

../output/fe_summary_$(MAIN_BW_M)m_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" Rscript border_pair_FE_regressions.R $(MAIN_BW_M) multifamily zonegroup_segment_year_additive ../output/fe_table_$(MAIN_BW_M)m_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex $(MAIN_YVARS)

../output/fe_table_$(MAIN_BW_M)m_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(MAIN_BW_M) all zonegroup_segment_year_additive "$@" $(MAIN_YVARS)

../output/fe_summary_$(MAIN_BW_M)m_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" Rscript border_pair_FE_regressions.R $(MAIN_BW_M) all zonegroup_segment_year_additive ../output/fe_table_$(MAIN_BW_M)m_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex $(MAIN_YVARS)

../output/fe_table_bw$(LEGACY_NARROW_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(LEGACY_NARROW_BW_FT) all zonegroup_segment_year_additive "$@" $(MAIN_YVARS)

../output/fe_table_bw$(LEGACY_NARROW_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(LEGACY_NARROW_BW_FT) all zonegroup_segment_year_additive "$@" $(MAIN_YVARS)

../output/fe_table_bw$(LEGACY_MAIN_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(LEGACY_MAIN_BW_FT) multifamily zonegroup_segment_year_additive "$@" $(MAIN_YVARS)

../output/fe_table_bw$(LEGACY_MAIN_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(LEGACY_MAIN_BW_FT) multifamily zonegroup_segment_year_additive "$@" $(MAIN_YVARS)

../output/fe_table_bw$(LEGACY_MAIN_BW_FT)_multifamily_zonegroup_pair_year_additive_clust_ward_pair.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(LEGACY_MAIN_BW_FT) multifamily zonegroup_pair_year_additive "$@" $(MAIN_YVARS)

../output/fe_table_bw$(LEGACY_NARROW_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(LEGACY_NARROW_BW_FT) multifamily zonegroup_segment_year_additive "$@" $(MAIN_YVARS)

../output/fe_summary_bw$(LEGACY_NARROW_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" Rscript border_pair_FE_regressions.R $(LEGACY_NARROW_BW_FT) all zonegroup_segment_year_additive ../output/fe_table_bw$(LEGACY_NARROW_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair.tex $(MAIN_YVARS)

../output/fe_summary_bw$(LEGACY_NARROW_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" Rscript border_pair_FE_regressions.R $(LEGACY_NARROW_BW_FT) multifamily zonegroup_segment_year_additive ../output/fe_table_bw$(LEGACY_NARROW_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex $(MAIN_YVARS)

../output/fe_summary_bw$(LEGACY_NARROW_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" Rscript border_pair_FE_regressions.R $(LEGACY_NARROW_BW_FT) all zonegroup_segment_year_additive ../output/fe_table_bw$(LEGACY_NARROW_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex $(MAIN_YVARS)

../output/fe_summary_bw$(LEGACY_MAIN_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" Rscript border_pair_FE_regressions.R $(LEGACY_MAIN_BW_FT) multifamily zonegroup_segment_year_additive ../output/fe_table_bw$(LEGACY_MAIN_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_pruned.tex $(MAIN_YVARS)

../output/fe_table_bw$(LEGACY_NARROW_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair_with_units_lotsize.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(LEGACY_NARROW_BW_FT) all zonegroup_segment_year_additive "$@" $(UNITS_LOTSIZE_YVARS)

../output/fe_table_bw$(LEGACY_NARROW_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_with_units_lotsize.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(LEGACY_NARROW_BW_FT) multifamily zonegroup_segment_year_additive "$@" $(UNITS_LOTSIZE_YVARS)

../output/fe_table_bw$(LEGACY_MAIN_BW_FT)_all_zonegroup_segment_year_additive_clust_ward_pair_with_units_lotsize.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(LEGACY_MAIN_BW_FT) all zonegroup_segment_year_additive "$@" $(UNITS_LOTSIZE_YVARS)

../output/fe_table_bw$(LEGACY_MAIN_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_with_units_lotsize.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(LEGACY_MAIN_BW_FT) multifamily zonegroup_segment_year_additive "$@" $(UNITS_LOTSIZE_YVARS)

../output/fe_table_bw$(LEGACY_WIDE_BW_FT)_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex: border_pair_FE_regressions.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(LEGACY_WIDE_BW_FT) multifamily zonegroup_segment_year_additive "$@" $(MAIN_YVARS)

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/confounded_segment_flags.csv: ../../border_confound_pruning/output/confounded_segment_flags.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv

include ../../generic.make
