SHELL := bash

MAIN_BW_M ?= 304.8
MAIN_BW_LABEL ?= 1000ft

all: \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf \
	../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf \
	../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(MAIN_BW_LABEL)_noctrl_geo_wardpair.tex

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 high_discretion issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 high_discretion issue ppml continuous_split uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 low_discretion_nosigns issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(MAIN_BW_LABEL)_noctrl_geo_wardpair.tex: did_table_permit_2015.R ../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< high_discretion $(MAIN_BW_M) uniform block "$@" none none cohort_2015

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet

include ../../shell_functions.make
include ../../generic.make
