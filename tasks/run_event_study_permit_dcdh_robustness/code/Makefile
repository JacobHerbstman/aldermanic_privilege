SHELL := bash
include ../../shell_functions.make

BW_FT := 1000
WEIGHTING := uniform
BOOTSTRAP_REPS := 0

all: \
	../output/event_study_dcdh_yearly_cohort_2015_high_discretion_issue_non_normalized_$(WEIGHTING)_$(BW_FT)ft_wardpair.pdf \
	../output/event_study_dcdh_yearly_cohort_2015_high_discretion_issue_normalized_$(WEIGHTING)_$(BW_FT)ft_wardpair.pdf \
	../output/summary_dcdh_yearly_cohort_2015_high_discretion_issue_$(WEIGHTING)_$(BW_FT)ft.csv \
	../output/summary_dcdh_yearly_cohort_2015_high_discretion_issue_$(WEIGHTING)_$(BW_FT)ft.tex

../output/summary_dcdh_yearly_cohort_2015_high_discretion_issue_$(WEIGHTING)_$(BW_FT)ft.tex: run_event_study_permit_dcdh_robustness.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript run_event_study_permit_dcdh_robustness.R $(BW_FT) $(WEIGHTING) $(BOOTSTRAP_REPS) \
		../output/event_study_dcdh_yearly_cohort_2015_high_discretion_issue_non_normalized_$(WEIGHTING)_$(BW_FT)ft_wardpair.pdf \
		../output/event_study_dcdh_yearly_cohort_2015_high_discretion_issue_normalized_$(WEIGHTING)_$(BW_FT)ft_wardpair.pdf \
		../output/summary_dcdh_yearly_cohort_2015_high_discretion_issue_$(WEIGHTING)_$(BW_FT)ft.csv \
		../output/summary_dcdh_yearly_cohort_2015_high_discretion_issue_$(WEIGHTING)_$(BW_FT)ft.tex

../output/event_study_dcdh_yearly_cohort_2015_high_discretion_issue_non_normalized_$(WEIGHTING)_$(BW_FT)ft_wardpair.pdf: ../output/summary_dcdh_yearly_cohort_2015_high_discretion_issue_$(WEIGHTING)_$(BW_FT)ft.tex

../output/event_study_dcdh_yearly_cohort_2015_high_discretion_issue_normalized_$(WEIGHTING)_$(BW_FT)ft_wardpair.pdf: ../output/summary_dcdh_yearly_cohort_2015_high_discretion_issue_$(WEIGHTING)_$(BW_FT)ft.tex

../output/summary_dcdh_yearly_cohort_2015_high_discretion_issue_$(WEIGHTING)_$(BW_FT)ft.csv: ../output/summary_dcdh_yearly_cohort_2015_high_discretion_issue_$(WEIGHTING)_$(BW_FT)ft.tex

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet

include ../../generic.make
