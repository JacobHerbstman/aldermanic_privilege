SHELL := bash
include ../../shell_functions.make

PANELS := cohort_2015 stacked_implementation
OUTCOMES := new_construction high_discretion
MAIN_WEIGHTINGS := triangular uniform

MAIN_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(foreach weighting,$(MAIN_WEIGHTINGS),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_$(weighting)_1000ft_within_block_full_clust_block.pdf)))
ROBUST_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_triangular_500ft_within_block_full_clust_block.pdf))
ROBUST_2000_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_triangular_2000ft_within_block_full_clust_block.pdf))
CONTROL_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_triangular_1000ft_within_block_full_clust_block_ctrl_baseline_demographics.pdf))
LOG_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(foreach weighting,$(MAIN_WEIGHTINGS),../output/event_study_yearly_$(panel)_$(outcome)_issue_log_continuous_$(weighting)_1000ft_within_block_full_clust_block.pdf)))
WARD_PAIR_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf))
WARD_PAIR_CONTROL_PDFS := $(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),../output/event_study_yearly_$(panel)_$(outcome)_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_ctrl_baseline_demographics_geo_wardpair.pdf))

all: $(MAIN_PDFS) $(ROBUST_PDFS) $(ROBUST_2000_PDFS) $(CONTROL_PDFS) $(LOG_PDFS)

define RUN_EVENT_STUDY_RULE
../output/event_study_yearly_$(1)_$(2)_issue_$(3)_continuous_$(4)_$(5)ft_within_block_full_clust_block$(7).pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/permit_block_year_panel_2015.parquet ../input/permit_block_year_panel_2023.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R $(1) $(2) issue $(3) continuous $(4) $(5) within_block full segment block $(6)
endef

$(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(foreach weighting,$(MAIN_WEIGHTINGS),$(eval $(call RUN_EVENT_STUDY_RULE,$(panel),$(outcome),ppml,$(weighting),1000,none,)))))
$(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(eval $(call RUN_EVENT_STUDY_RULE,$(panel),$(outcome),ppml,triangular,500,none,))))
$(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(eval $(call RUN_EVENT_STUDY_RULE,$(panel),$(outcome),ppml,triangular,2000,none,))))
$(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(eval $(call RUN_EVENT_STUDY_RULE,$(panel),$(outcome),ppml,triangular,1000,baseline_demographics,_ctrl_baseline_demographics))))
$(foreach panel,$(PANELS),$(foreach outcome,$(OUTCOMES),$(foreach weighting,$(MAIN_WEIGHTINGS),$(eval $(call RUN_EVENT_STUDY_RULE,$(panel),$(outcome),log,$(weighting),1000,none,)))))

../output/did_table_high_discretion_issue_ppml_uniform_1000ft_full.tex: run_did_permit.R ../input/permit_block_year_panel.parquet ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_did_permit.R high_discretion issue ppml uniform 1000 full

../output/did_table_high_discretion_issue_ppml_uniform_1000ft_full_geo_wardpair.tex: run_did_permit.R ../input/permit_block_year_panel.parquet ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_did_permit.R high_discretion issue ppml uniform 1000 full ward_pair

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform 1000 within_block full ward_pair block none

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml continuous uniform 1000 within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_500ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform 500 within_block full ward_pair block none

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_500ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml continuous uniform 500 within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_new_construction_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 new_construction issue ppml continuous uniform 1000 within_block full ward_pair block none

../output/event_study_yearly_stacked_implementation_new_construction_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation new_construction issue ppml continuous uniform 1000 within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_ctrl_baseline_demographics_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform 1000 within_block full ward_pair block baseline_demographics

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_ctrl_baseline_demographics_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml continuous uniform 1000 within_block full ward_pair block baseline_demographics

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_binary_direction_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_cohort_2015_high_discretion_issue_ppml_binary_direction_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml binary_direction uniform 1000 within_block full ward_pair block none

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_binary_direction_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_stacked_implementation_high_discretion_issue_ppml_binary_direction_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml binary_direction uniform 1000 within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous_split uniform 1000 within_block full ward_pair block none

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_split_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_split_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml continuous_split uniform 1000 within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_500ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_500ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous_split uniform 500 within_block full ward_pair block none

../output/event_study_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_split_uniform_500ft_within_block_full_clust_block_geo_wardpair.pdf ../output/event_study_combined_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_split_uniform_500ft_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation high_discretion issue ppml continuous_split uniform 500 within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_new_construction_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_ctrl_baseline_demographics_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 new_construction issue ppml continuous uniform 1000 within_block full ward_pair block baseline_demographics

../output/event_study_yearly_stacked_implementation_new_construction_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_ctrl_baseline_demographics_geo_wardpair.pdf: run_event_study_permit.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation new_construction issue ppml continuous uniform 1000 within_block full ward_pair block baseline_demographics

../input/permit_block_year_panel.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel.parquet | ../input
	ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

../input/permit_block_year_panel_2023.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2023.parquet | ../input
	ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel.parquet ../input/permit_block_year_panel_2015.parquet ../input/permit_block_year_panel_2023.parquet ../input/block_group_controls.csv

include ../../generic.make
