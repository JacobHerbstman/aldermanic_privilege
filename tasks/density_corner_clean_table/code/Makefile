SHELL := bash

BW_M ?= 100
BW_FT := $(shell Rscript -e 'cat(round($(BW_M) / 0.3048))')
SAMPLE ?= all

all: ../output/fe_table_$(BW_M)m_$(SAMPLE)_corner_clean_compare.tex

../output/fe_summary_$(BW_M)m_$(SAMPLE)_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R $(BW_FT) $(SAMPLE) zonegroup_segment_year_additive ../output/ignore_$(BW_M)m_$(SAMPLE)_baseline.tex 'log(density_far)' 'log(density_dupac)'

../output/fe_summary_$(BW_M)m_$(SAMPLE)_zonegroup_segment_year_additive_clust_ward_pair_corner_clean.csv: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv ../input/parcel_other_pair_distance.csv | ../output
	DROP_AMBIGUOUS_WITHIN_BW=TRUE AMBIGUITY_INPUT_PATH=../input/parcel_other_pair_distance.csv WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH="$@" CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R $(BW_FT) $(SAMPLE) zonegroup_segment_year_additive ../output/ignore_$(BW_M)m_$(SAMPLE)_corner_clean.tex 'log(density_far)' 'log(density_dupac)'

../output/fe_table_$(BW_M)m_$(SAMPLE)_corner_clean_compare.tex: build_density_corner_clean_table.R ../output/fe_summary_$(BW_M)m_$(SAMPLE)_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../output/fe_summary_$(BW_M)m_$(SAMPLE)_zonegroup_segment_year_additive_clust_ward_pair_corner_clean.csv ../input/boundary_ambiguity_by_bw.csv | ../output
	Rscript build_density_corner_clean_table.R ../output/fe_summary_$(BW_M)m_$(SAMPLE)_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../output/fe_summary_$(BW_M)m_$(SAMPLE)_zonegroup_segment_year_additive_clust_ward_pair_corner_clean.csv ../input/boundary_ambiguity_by_bw.csv "$@" $(BW_FT) $(SAMPLE)

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcel_other_pair_distance.csv: ../../border_pair_boundary_ambiguity/output/parcel_other_pair_distance.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_ambiguity_by_bw.csv: ../../border_pair_boundary_ambiguity/output/boundary_ambiguity_by_bw.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/parcel_other_pair_distance.csv ../input/boundary_ambiguity_by_bw.csv

include ../../generic.make
