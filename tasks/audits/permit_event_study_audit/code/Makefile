MAIN_BW_M ?= 152.4
MAIN_BW_LABEL ?= 500ft

all: \
	../output/event_study_coefficients_yearly_cohort_2015_high_discretion_application_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv \
	../output/event_study_coefficients_yearly_cohort_2015_high_discretion_application_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv \
	../output/event_study_coefficients_yearly_cohort_2015_low_discretion_nosigns_application_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv

../output/event_study_coefficients_yearly_cohort_2015_high_discretion_application_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 high_discretion application ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair ward_pair pre_high_level none $(MAIN_BW_LABEL)

../output/event_study_coefficients_yearly_cohort_2015_high_discretion_application_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 high_discretion application ppml continuous_split uniform $(MAIN_BW_M) within_block full ward_pair ward_pair pre_high_level none $(MAIN_BW_LABEL)

../output/event_study_coefficients_yearly_cohort_2015_low_discretion_nosigns_application_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_wardpair_ctrl_pre_high_level_geo_wardpair.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 low_discretion_nosigns application ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair ward_pair pre_high_level none $(MAIN_BW_LABEL)

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet

include ../../../shell_functions.make
include ../../../generic.make
