MAIN_BW_M ?= 152.4
MAIN_BW_LABEL ?= 500ft
SCORE_VARIANTS := current_no_income education_added_back income_added_back all_covariates
OUTCOMES := high_discretion low_discretion_nosigns

all: \
	../output/event_study_coefficients_yearly_cohort_2015_high_discretion_application_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv \
	../output/event_study_coefficients_yearly_cohort_2015_high_discretion_application_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv \
	../output/event_study_coefficients_yearly_cohort_2015_low_discretion_nosigns_application_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv \
	$(foreach outcome,$(OUTCOMES),$(foreach score,$(SCORE_VARIANTS),../output/binary_event_study_pooled_$(outcome)_$(score)_$(MAIN_BW_LABEL).csv)) \
	$(foreach outcome,$(OUTCOMES),$(foreach score,$(SCORE_VARIANTS),../output/binary_joint_directional_event_study_$(outcome)_$(score)_$(MAIN_BW_LABEL).pdf)) \
	../output/binary_joint_directional_event_study_high_discretion_income_added_back_$(MAIN_BW_LABEL)_paper.pdf \
	../output/binary_signed_directional_event_study_high_discretion_income_added_back_$(MAIN_BW_LABEL)_paper.pdf \
	../output/binary_signed_directional_event_study_low_discretion_nosigns_income_added_back_$(MAIN_BW_LABEL)_paper.pdf \
	../output/binary_signed_directional_event_study_high_discretion_income_added_back_$(MAIN_BW_LABEL)_stable_paper.pdf \
	../output/binary_event_study_appendix_income_added_back_$(MAIN_BW_LABEL).tex \
	$(foreach outcome,$(OUTCOMES),$(foreach score,$(SCORE_VARIANTS),../output/binary_aggregate_event_study_$(outcome)_$(score)_$(MAIN_BW_LABEL).pdf))

../output/event_study_coefficients_yearly_cohort_2015_high_discretion_application_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 high_discretion application ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair ward_pair pre_high_level none $(MAIN_BW_LABEL)

../output/event_study_coefficients_yearly_cohort_2015_high_discretion_application_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 high_discretion application ppml continuous_split uniform $(MAIN_BW_M) within_block full ward_pair ward_pair pre_high_level none $(MAIN_BW_LABEL)

../output/event_study_coefficients_yearly_cohort_2015_low_discretion_nosigns_application_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 low_discretion_nosigns application ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair ward_pair pre_high_level none $(MAIN_BW_LABEL)

define binary_event_study_rule
../output/binary_event_study_pooled_$(1)_$(2)_$(MAIN_BW_LABEL).csv: run_binary_aggregate_event_study.R ../input/permit_block_year_panel_2015.parquet ../input/score_control_variants.csv | ../output
	$$(R) $$< $$(MAIN_BW_M) $$(MAIN_BW_LABEL) $(2) $(1)
endef

$(foreach outcome,$(OUTCOMES),$(foreach score,$(SCORE_VARIANTS),$(eval $(call binary_event_study_rule,$(outcome),$(score)))))

../output/binary_joint_event_study_%_$(MAIN_BW_LABEL).csv \
../output/binary_signed_event_study_%_$(MAIN_BW_LABEL).csv \
../output/binary_joint_directional_event_study_%_$(MAIN_BW_LABEL).pdf \
../output/binary_joint_directional_event_study_%_$(MAIN_BW_LABEL).png \
../output/binary_aggregate_event_study_%_$(MAIN_BW_LABEL).pdf \
../output/binary_aggregate_event_study_%_$(MAIN_BW_LABEL).png: \
	../output/binary_event_study_pooled_%_$(MAIN_BW_LABEL).csv

../output/binary_joint_directional_event_study_high_discretion_income_added_back_$(MAIN_BW_LABEL)_paper.pdf: \
	run_binary_aggregate_event_study.R \
	../input/permit_block_year_panel_2015.parquet \
	../input/score_control_variants.csv | ../output
	$(R) $< $(MAIN_BW_M) $(MAIN_BW_LABEL) income_added_back high_discretion

../output/binary_signed_directional_event_study_high_discretion_income_added_back_$(MAIN_BW_LABEL)_paper.pdf: \
	run_binary_aggregate_event_study.R \
	../input/permit_block_year_panel_2015.parquet \
	../input/score_control_variants.csv | ../output
	$(R) $< $(MAIN_BW_M) $(MAIN_BW_LABEL) income_added_back high_discretion

../output/binary_signed_directional_event_study_low_discretion_nosigns_income_added_back_$(MAIN_BW_LABEL)_paper.pdf: \
	run_binary_aggregate_event_study.R \
	../input/permit_block_year_panel_2015.parquet \
	../input/score_control_variants.csv | ../output
	$(R) $< $(MAIN_BW_M) $(MAIN_BW_LABEL) income_added_back low_discretion_nosigns

../output/binary_signed_directional_event_study_high_discretion_income_added_back_$(MAIN_BW_LABEL)_stable_paper.pdf: \
	run_binary_aggregate_event_study.R \
	../input/permit_block_year_panel_2015.parquet \
	../input/score_control_variants.csv | ../output
	$(R) $< $(MAIN_BW_M) $(MAIN_BW_LABEL) income_added_back high_discretion stable

../output/binary_event_study_appendix_income_added_back_$(MAIN_BW_LABEL).tex: \
	build_binary_event_study_appendix_table.R \
	../output/binary_event_study_pooled_high_discretion_income_added_back_$(MAIN_BW_LABEL).csv \
	../output/binary_event_study_pooled_low_discretion_nosigns_income_added_back_$(MAIN_BW_LABEL).csv | ../output
	$(R) $< $(MAIN_BW_LABEL)

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/score_control_variants.csv: ../../income_score_current_results/output/current_income_scores.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/score_control_variants.csv

include ../../../shell_functions.make
include ../../../generic.make
