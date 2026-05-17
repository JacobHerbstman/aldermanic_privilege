SHELL := bash

VARIANTS := baseline raw_rank_days days_unlogged reduced_ses drop_bach_pop
RESIDUALIZED_VARIANTS := baseline days_unlogged reduced_ses drop_bach_pop
BASELINE_SPEC := ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022
DENSITY_ROBUSTNESS_BW_M ?= 100
DENSITY_ROBUSTNESS_SAMPLE := all
DENSITY_ROBUSTNESS_CLUSTER := ward_pair
DENSITY_ROBUSTNESS_STEM := $(DENSITY_ROBUSTNESS_BW_M)m_$(DENSITY_ROBUSTNESS_SAMPLE)_zonegroup_segment_year_additive_clust_$(DENSITY_ROBUSTNESS_CLUSTER)
DENSITY_ROBUSTNESS_ENV := DENSITY_ROBUSTNESS_BW_M=$(DENSITY_ROBUSTNESS_BW_M) DENSITY_ROBUSTNESS_SAMPLE=$(DENSITY_ROBUSTNESS_SAMPLE) DENSITY_ROBUSTNESS_CLUSTER=$(DENSITY_ROBUSTNESS_CLUSTER)

SCORE_OUTPUTS := $(foreach variant,$(VARIANTS),../output/alderman_uncertainty_index_$(variant).csv)
METADATA_OUTPUTS := $(foreach variant,$(VARIANTS),../output/score_variant_metadata_$(variant).csv)
STAGE1_TERM_VARIANT_OUTPUTS := $(foreach variant,$(VARIANTS),../output/score_variant_stage1_terms_$(variant).csv)
STAGE1_TABLE_OUTPUTS := $(foreach variant,$(RESIDUALIZED_VARIANTS),../output/stage1_regression_$(variant).tex)
STAGE2_TABLE_OUTPUTS := $(foreach variant,$(RESIDUALIZED_VARIANTS),../output/stage2_regression_$(variant).tex)
MERGED_OUTPUTS := $(foreach variant,$(VARIANTS),../output/parcels_with_ward_distances_$(variant).csv)
MERGE_SUMMARY_OUTPUTS := $(foreach variant,$(VARIANTS),../output/boundary_distance_summary_$(variant).csv)
FE_TABLE_OUTPUTS := $(foreach variant,$(VARIANTS),../output/fe_table_$(DENSITY_ROBUSTNESS_STEM)_$(variant).tex)
FE_SUMMARY_OUTPUTS := $(foreach variant,$(VARIANTS),../output/fe_summary_$(DENSITY_ROBUSTNESS_STEM)_$(variant).csv)

all: $(SCORE_OUTPUTS) $(METADATA_OUTPUTS) $(STAGE1_TERM_VARIANT_OUTPUTS) $(STAGE1_TABLE_OUTPUTS) $(STAGE2_TABLE_OUTPUTS) $(MERGED_OUTPUTS) $(MERGE_SUMMARY_OUTPUTS) $(FE_TABLE_OUTPUTS) $(FE_SUMMARY_OUTPUTS) \
	../output/score_variant_diagnostics.csv \
	../output/score_variant_diagnostics.tex \
	../output/score_variant_rank_changes.csv \
	../output/score_variant_stage1_terms.csv \
	../output/density_score_robustness_summary.csv \
	../output/density_score_robustness_summary.tex

define RESIDUALIZED_SCORE_RULE
../output/alderman_uncertainty_index_$(1).csv: build_score_variants.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../output
	Rscript build_score_variants.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv $(1) ../output/alderman_uncertainty_index_$(1).csv ../output/score_variant_metadata_$(1).csv ../output/score_variant_stage1_terms_$(1).csv ../output/stage1_regression_$(1).tex ../output/stage2_regression_$(1).tex
../output/score_variant_metadata_$(1).csv: ../output/alderman_uncertainty_index_$(1).csv
	@test -s "$$@"

../output/score_variant_stage1_terms_$(1).csv ../output/stage1_regression_$(1).tex \
../output/stage2_regression_$(1).tex: ../output/score_variant_metadata_$(1).csv
	@test -s "$$@"
endef
$(foreach variant,$(RESIDUALIZED_VARIANTS),$(eval $(call RESIDUALIZED_SCORE_RULE,$(variant))))

../output/alderman_uncertainty_index_raw_rank_days.csv: build_score_variants.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../output
	Rscript build_score_variants.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv raw_rank_days ../output/alderman_uncertainty_index_raw_rank_days.csv ../output/score_variant_metadata_raw_rank_days.csv ../output/score_variant_stage1_terms_raw_rank_days.csv NA NA

../output/score_variant_metadata_raw_rank_days.csv: ../output/alderman_uncertainty_index_raw_rank_days.csv
	@test -s "$@"

../output/score_variant_stage1_terms_raw_rank_days.csv: ../output/score_variant_metadata_raw_rank_days.csv
	@test -s "$@"
define MERGE_RULE
../output/parcels_with_ward_distances_$(1).csv: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../output/alderman_uncertainty_index_$(1).csv | ../output
	PARCELS_INPUT_PATH=../input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances_$(1).csv MERGE_SUMMARY_OUTPUT_PATH=../output/boundary_distance_summary_$(1).csv Rscript ../../merge_in_scores/code/merge_scores.R ../output/alderman_uncertainty_index_$(1).csv uncertainty_index
../output/boundary_distance_summary_$(1).csv: ../output/parcels_with_ward_distances_$(1).csv
	@test -s "$$@"
endef
$(foreach variant,$(VARIANTS),$(eval $(call MERGE_RULE,$(variant))))

define FE_RULE
../output/fe_table_$(DENSITY_ROBUSTNESS_STEM)_$(1).tex: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_$(1).csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_$(1).csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_$(DENSITY_ROBUSTNESS_STEM)_$(1).csv CLUSTER_LEVEL=$(DENSITY_ROBUSTNESS_CLUSTER) Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R $(DENSITY_ROBUSTNESS_BW_M) $(DENSITY_ROBUSTNESS_SAMPLE) zonegroup_segment_year_additive ../output/fe_table_$(DENSITY_ROBUSTNESS_STEM)_$(1).tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
../output/fe_summary_$(DENSITY_ROBUSTNESS_STEM)_$(1).csv: ../output/fe_table_$(DENSITY_ROBUSTNESS_STEM)_$(1).tex
	@test -s "$$@"
endef
$(foreach variant,$(VARIANTS),$(eval $(call FE_RULE,$(variant))))

../output/score_variant_diagnostics.csv: summarize_score_robustness.R $(SCORE_OUTPUTS) $(METADATA_OUTPUTS) \
	$(STAGE1_TERM_VARIANT_OUTPUTS) $(MERGED_OUTPUTS) $(FE_SUMMARY_OUTPUTS) | ../output
	$(DENSITY_ROBUSTNESS_ENV) Rscript summarize_score_robustness.R ../output

../output/score_variant_diagnostics.tex ../output/score_variant_rank_changes.csv \
../output/score_variant_stage1_terms.csv ../output/density_score_robustness_summary.csv \
../output/density_score_robustness_summary.tex: ../output/score_variant_diagnostics.csv
	@test -s "$@"

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv

include ../../shell_functions.make
include ../../generic.make
