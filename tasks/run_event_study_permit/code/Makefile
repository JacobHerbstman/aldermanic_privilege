SHELL := bash
include ../../shell_functions.make

PANELS := cohort_2015 stacked_implementation
OUTCOMES := high_discretion
MAIN_WEIGHTINGS := triangular uniform
MAIN_BW_M ?= 250
MAIN_BW_FT := $(shell Rscript -e 'cat(round($(MAIN_BW_M) / 0.3048))')
MAIN_BW_LABEL ?= $(MAIN_BW_M)m
LEGACY_MAIN_BW_FT ?= 1000
LEGACY_ROBUST_BW_FT ?= 500
LEGACY_WIDE_BW_FT ?= 2000

MAIN_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(foreach weighting,$(MAIN_WEIGHTINGS),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_$(weighting)_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block.pdf)))
ROBUST_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_triangular_$(LEGACY_ROBUST_BW_FT)ft_within_block_full_clust_block.pdf))
ROBUST_2000_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_triangular_$(LEGACY_WIDE_BW_FT)ft_within_block_full_clust_block.pdf))
CONTROL_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_triangular_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_ctrl_baseline_demographics.pdf))
LOG_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(foreach weighting,$(MAIN_WEIGHTINGS),../output/event_study_yearly_$(panel)_$(outcome)_issue_log_continuous_$(weighting)_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block.pdf)))
WARD_PAIR_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf))
WARD_PAIR_CONTROL_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_ctrl_baseline_demographics_geo_wardpair.pdf))
PAPER_FIGURES := \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf \
	../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf
PAPER_TABLES := ../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(MAIN_BW_LABEL)_geo_wardpair.tex
LOW_DISCRETION_PLACEBO_OUTPUTS := \
	../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf \
	../output/event_study_yearly_stacked_implementation_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf \
	../output/did_table_permit_2015_low_discretion_nosigns_issue_ppml_uniform_$(MAIN_BW_LABEL)_geo_wardpair.tex
BG_ROBUSTNESS_OUTPUTS := \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_singlepairbg.pdf \
	../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_singlepairbg.csv \
	../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_singlepairbg.csv \
	../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_singlepairbg.csv \
	../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_singlepairbg.csv \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_unmixedtreatbg.pdf \
	../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_unmixedtreatbg.csv \
	../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_unmixedtreatbg.csv \
	../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_unmixedtreatbg.csv \
	../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_unmixedtreatbg.csv \
	../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(LEGACY_MAIN_BW_FT)ft_geo_wardpair_samp_singlepairbg.tex \
	../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(LEGACY_MAIN_BW_FT)ft_geo_wardpair_samp_unmixedtreatbg.tex

all: $(PAPER_FIGURES) $(PAPER_TABLES)

low_discretion_placebo: $(LOW_DISCRETION_PLACEBO_OUTPUTS)

bg_robustness: $(BG_ROBUSTNESS_OUTPUTS)

define RUN_EVENT_STUDY_RULE
../output/event_study_yearly_$(1)_$(2)_issue_$(3)_continuous_$(4)_$(5)ft_within_block_full_clust_block$(7).pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/permit_block_year_panel_2015.parquet ../input/permit_block_year_panel_2023.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R $(1) $(2) issue $(3) continuous $(4) $(5) within_block full segment block $(6)
endef

$(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(foreach weighting,$(MAIN_WEIGHTINGS),$(eval $(call RUN_EVENT_STUDY_RULE,$(panel),$(outcome),ppml,$(weighting),$(LEGACY_MAIN_BW_FT),none,)))))
$(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(eval $(call RUN_EVENT_STUDY_RULE,$(panel),$(outcome),ppml,triangular,$(LEGACY_ROBUST_BW_FT),none,))))
$(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(eval $(call RUN_EVENT_STUDY_RULE,$(panel),$(outcome),ppml,triangular,$(LEGACY_WIDE_BW_FT),none,))))
$(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(eval $(call RUN_EVENT_STUDY_RULE,$(panel),$(outcome),ppml,triangular,$(LEGACY_MAIN_BW_FT),baseline_demographics,_ctrl_baseline_demographics))))
$(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(foreach weighting,$(MAIN_WEIGHTINGS),$(eval $(call RUN_EVENT_STUDY_RULE,$(panel),$(outcome),log,$(weighting),$(LEGACY_MAIN_BW_FT),none,)))))

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform $(MAIN_BW_FT) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous_split uniform $(MAIN_BW_FT) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 low_discretion_nosigns issue ppml continuous uniform $(MAIN_BW_FT) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_stacked_implementation_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation low_discretion_nosigns issue ppml continuous uniform $(MAIN_BW_FT) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(MAIN_BW_LABEL)_geo_wardpair.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript did_table_permit_2015.R high_discretion $(MAIN_BW_FT) uniform block $@

../output/did_table_permit_2015_low_discretion_nosigns_issue_ppml_uniform_$(MAIN_BW_LABEL)_geo_wardpair.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript did_table_permit_2015.R low_discretion_nosigns $(MAIN_BW_FT) uniform block $@

../output/did_table_high_discretion_issue_ppml_uniform_$(LEGACY_MAIN_BW_FT)ft_full.tex: run_did_permit.R ../input/permit_block_year_panel.parquet ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_did_permit.R high_discretion issue ppml uniform $(LEGACY_MAIN_BW_FT) full

../output/did_table_high_discretion_issue_ppml_uniform_$(LEGACY_MAIN_BW_FT)ft_full_geo_wardpair.tex: run_did_permit.R ../input/permit_block_year_panel.parquet ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_did_permit.R high_discretion issue ppml uniform $(LEGACY_MAIN_BW_FT) full ward_pair

../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(LEGACY_MAIN_BW_FT)ft_geo_wardpair.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript did_table_permit_2015.R high_discretion $(LEGACY_MAIN_BW_FT) uniform block $@

../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(LEGACY_ROBUST_BW_FT)ft_geo_wardpair.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript did_table_permit_2015.R high_discretion $(LEGACY_ROBUST_BW_FT) uniform block $@

../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(LEGACY_MAIN_BW_FT)ft_geo_wardpair_samp_singlepairbg.tex: did_table_permit_2015.R ../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript did_table_permit_2015.R high_discretion $(LEGACY_MAIN_BW_FT) uniform block $@ single_pair_bg

../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(LEGACY_MAIN_BW_FT)ft_geo_wardpair_samp_unmixedtreatbg.tex: did_table_permit_2015.R ../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript did_table_permit_2015.R high_discretion $(LEGACY_MAIN_BW_FT) uniform block $@ unmixed_treat_bg

../output/did_table_permit_2015_low_discretion_nosigns_issue_ppml_uniform_$(LEGACY_MAIN_BW_FT)ft_geo_wardpair.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript did_table_permit_2015.R low_discretion_nosigns $(LEGACY_MAIN_BW_FT) uniform block $@

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_singlepairbg.pdf \
../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_singlepairbg.csv \
../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_singlepairbg.csv \
../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_singlepairbg.csv \
../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_singlepairbg.csv: run_event_study_permit.R ../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	WRITE_SIDECARS=1 Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block none single_pair_bg

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_unmixedtreatbg.pdf \
../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_unmixedtreatbg.csv \
../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_unmixedtreatbg.csv \
../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_unmixedtreatbg.csv \
../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair_samp_unmixedtreatbg.csv: run_event_study_permit.R ../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	WRITE_SIDECARS=1 Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block none unmixed_treat_bg

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml continuous uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_ROBUST_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform $(LEGACY_ROBUST_BW_FT) within_block full ward_pair block none

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_ROBUST_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml continuous uniform $(LEGACY_ROBUST_BW_FT) within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 low_discretion_nosigns issue ppml continuous uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block none

../output/event_study_yearly_stacked_implementation_low_discretion_nosigns_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation low_discretion_nosigns issue ppml continuous uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_ctrl_baseline_demographics_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block baseline_demographics

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_ctrl_baseline_demographics_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml continuous uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block baseline_demographics

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_binary_direction_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_cohort_2015_high_discretion_issue_ppml_binary_direction_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml binary_direction uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block none

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_binary_direction_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_stacked_implementation_high_discretion_issue_ppml_binary_direction_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml binary_direction uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous_split uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block none

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_split_uniform_$(LEGACY_MAIN_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml continuous_split uniform $(LEGACY_MAIN_BW_FT) within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(LEGACY_ROBUST_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(LEGACY_ROBUST_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous_split uniform $(LEGACY_ROBUST_BW_FT) within_block full ward_pair block none

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_split_uniform_$(LEGACY_ROBUST_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_split_uniform_$(LEGACY_ROBUST_BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml continuous_split uniform $(LEGACY_ROBUST_BW_FT) within_block full ward_pair block none

../input/permit_block_year_panel.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2023.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2023.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv

include ../../generic.make
