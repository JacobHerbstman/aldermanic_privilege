SHELL := bash
include ../../shell_functions.make

VARIANTS := baseline raw_rank_days days_unlogged reduced_ses drop_bach drop_bach_pop
RESIDUALIZED_VARIANTS := baseline days_unlogged reduced_ses drop_bach drop_bach_pop
BASELINE_SPEC := ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH

SCORE_OUTPUTS := $(foreach variant,$(VARIANTS),../output/alderman_uncertainty_index_$(variant).csv)
METADATA_OUTPUTS := $(foreach variant,$(VARIANTS),../output/score_variant_metadata_$(variant).csv)
STAGE1_TERM_VARIANT_OUTPUTS := $(foreach variant,$(VARIANTS),../output/score_variant_stage1_terms_$(variant).csv)
STAGE1_TABLE_OUTPUTS := $(foreach variant,$(RESIDUALIZED_VARIANTS),../output/stage1_regression_$(variant).tex)
STAGE2_TABLE_OUTPUTS := $(foreach variant,$(RESIDUALIZED_VARIANTS),../output/stage2_regression_$(variant).tex)
MERGED_OUTPUTS := $(foreach variant,$(VARIANTS),../output/parcels_with_ward_distances_$(variant).csv)
MERGE_SUMMARY_OUTPUTS := $(foreach variant,$(VARIANTS),../output/boundary_distance_summary_$(variant).csv)
RD_OUTPUTS := $(foreach variant,$(VARIANTS),../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_segment_$(variant).pdf ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_segment_$(variant).pdf)
FE_TABLE_OUTPUTS := $(foreach variant,$(VARIANTS),../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment_$(variant).tex)
FE_SUMMARY_OUTPUTS := $(foreach variant,$(VARIANTS),../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_segment_$(variant).csv)

all: $(SCORE_OUTPUTS) $(METADATA_OUTPUTS) $(STAGE1_TERM_VARIANT_OUTPUTS) $(STAGE1_TABLE_OUTPUTS) $(STAGE2_TABLE_OUTPUTS) $(MERGED_OUTPUTS) $(MERGE_SUMMARY_OUTPUTS) $(RD_OUTPUTS) $(FE_TABLE_OUTPUTS) $(FE_SUMMARY_OUTPUTS) \
	../output/score_variant_diagnostics.csv \
	../output/score_variant_diagnostics.tex \
	../output/score_variant_rank_changes.csv \
	../output/score_variant_stage1_terms.csv \
	../output/density_score_robustness_summary.csv \
	../output/density_score_robustness_summary.tex

define RESIDUALIZED_SCORE_RULE
../output/alderman_uncertainty_index_$(1).csv ../output/score_variant_metadata_$(1).csv ../output/score_variant_stage1_terms_$(1).csv ../output/stage1_regression_$(1).tex ../output/stage2_regression_$(1).tex: build_score_variants.R ../../create_alderman_uncertainty_index/code/uncertainty_index_helpers.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../output
	Rscript build_score_variants.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv $(1) ../output/alderman_uncertainty_index_$(1).csv ../output/score_variant_metadata_$(1).csv ../output/score_variant_stage1_terms_$(1).csv ../output/stage1_regression_$(1).tex ../output/stage2_regression_$(1).tex
endef
$(foreach variant,$(RESIDUALIZED_VARIANTS),$(eval $(call RESIDUALIZED_SCORE_RULE,$(variant))))

../output/alderman_uncertainty_index_raw_rank_days.csv ../output/score_variant_metadata_raw_rank_days.csv ../output/score_variant_stage1_terms_raw_rank_days.csv: build_score_variants.R ../../create_alderman_uncertainty_index/code/uncertainty_index_helpers.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../output
	Rscript build_score_variants.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv raw_rank_days ../output/alderman_uncertainty_index_raw_rank_days.csv ../output/score_variant_metadata_raw_rank_days.csv ../output/score_variant_stage1_terms_raw_rank_days.csv NA NA

define MERGE_RULE
../output/parcels_with_ward_distances_$(1).csv ../output/boundary_distance_summary_$(1).csv: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../output/alderman_uncertainty_index_$(1).csv | ../output
	PARCELS_INPUT_PATH=../input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances_$(1).csv MERGE_SUMMARY_OUTPUT_PATH=../output/boundary_distance_summary_$(1).csv Rscript ../../merge_in_scores/code/merge_scores.R ../output/alderman_uncertainty_index_$(1).csv uncertainty_index
endef
$(foreach variant,$(VARIANTS),$(eval $(call MERGE_RULE,$(variant))))

define RD_FAR_RULE
../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_segment_$(1).pdf: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_$(1).csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_$(1).csv CLUSTER_LEVEL=segment Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 500 multifamily zonegroup_segment_year_additive $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef
$(foreach variant,$(VARIANTS),$(eval $(call RD_FAR_RULE,$(variant))))

define RD_DUPAC_RULE
../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_segment_$(1).pdf: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_$(1).csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_$(1).csv CLUSTER_LEVEL=segment Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 500 multifamily zonegroup_segment_year_additive $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef
$(foreach variant,$(VARIANTS),$(eval $(call RD_DUPAC_RULE,$(variant))))

define FE_RULE
../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment_$(1).tex ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_segment_$(1).csv: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_$(1).csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_$(1).csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_segment_$(1).csv CLUSTER_LEVEL=segment Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment_$(1).tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
endef
$(foreach variant,$(VARIANTS),$(eval $(call FE_RULE,$(variant))))

../output/score_variant_diagnostics.csv \
../output/score_variant_diagnostics.tex \
../output/score_variant_rank_changes.csv \
../output/score_variant_stage1_terms.csv \
../output/density_score_robustness_summary.csv \
../output/density_score_robustness_summary.tex: summarize_score_robustness.R $(SCORE_OUTPUTS) $(METADATA_OUTPUTS) $(STAGE1_TERM_VARIANT_OUTPUTS) $(MERGED_OUTPUTS) $(FE_SUMMARY_OUTPUTS) | ../output
	Rscript summarize_score_robustness.R ../output

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf "$<" "$@"

../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../input
	ln -sf "$<" "$@"

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv

include ../../generic.make
