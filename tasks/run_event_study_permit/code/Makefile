SHELL := bash

MAIN_BW_M ?= 304.8
MAIN_BW_LABEL ?= 1000ft

PAPER_FIGURES := \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf \
	../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf
PAPER_TABLES := ../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(MAIN_BW_LABEL)_noctrl_geo_wardpair.tex
LOW_DISCRETION_PLACEBO_OUTPUTS := \
	../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf \
	../output/event_study_yearly_stacked_implementation_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf \
	../output/did_table_permit_2015_low_discretion_nosigns_issue_ppml_uniform_$(MAIN_BW_LABEL)_geo_wardpair.tex
SEGMENT_LENGTH_ROBUSTNESS_OUTPUTS := \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.pdf \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.pdf \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.pdf \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.pdf \
	../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.pdf \
	../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.pdf \
	../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(MAIN_BW_LABEL)_noctrl_geo_wardpair_minsegment500ft.tex \
	../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(MAIN_BW_LABEL)_noctrl_geo_wardpair_minsegment1000ft.tex \
	../output/did_table_permit_2015_low_discretion_nosigns_issue_ppml_uniform_$(MAIN_BW_LABEL)_geo_wardpair_minsegment500ft.tex \
	../output/did_table_permit_2015_low_discretion_nosigns_issue_ppml_uniform_$(MAIN_BW_LABEL)_geo_wardpair_minsegment1000ft.tex

all: $(PAPER_FIGURES) $(PAPER_TABLES)

low_discretion_placebo: $(LOW_DISCRETION_PLACEBO_OUTPUTS)

segment_length_robustness: $(SEGMENT_LENGTH_ROBUSTNESS_OUTPUTS)

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	WRITE_SIDECARS=1 Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	MIN_SEGMENT_LENGTH_FT=500 WRITE_SIDECARS=1 Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	MIN_SEGMENT_LENGTH_FT=1000 WRITE_SIDECARS=1 Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous_split uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	MIN_SEGMENT_LENGTH_FT=500 Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous_split uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	MIN_SEGMENT_LENGTH_FT=1000 Rscript run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous_split uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_combined_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: ../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf
	@test -s "$@"

../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R cohort_2015 low_discretion_nosigns issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	MIN_SEGMENT_LENGTH_FT=500 Rscript run_event_study_permit.R cohort_2015 low_discretion_nosigns issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	MIN_SEGMENT_LENGTH_FT=1000 Rscript run_event_study_permit.R cohort_2015 low_discretion_nosigns issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_yearly_stacked_implementation_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.pdf: run_event_study_permit.R ../../_lib/event_study_plot_helpers.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_event_study_permit.R stacked_implementation low_discretion_nosigns issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(MAIN_BW_LABEL)_noctrl_geo_wardpair.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript did_table_permit_2015.R high_discretion $(MAIN_BW_M) uniform block "$@" none

../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(MAIN_BW_LABEL)_noctrl_geo_wardpair_minsegment500ft.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	MIN_SEGMENT_LENGTH_FT=500 Rscript did_table_permit_2015.R high_discretion $(MAIN_BW_M) uniform block "$@" none

../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_$(MAIN_BW_LABEL)_noctrl_geo_wardpair_minsegment1000ft.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	MIN_SEGMENT_LENGTH_FT=1000 Rscript did_table_permit_2015.R high_discretion $(MAIN_BW_M) uniform block "$@" none

../output/did_table_permit_2015_low_discretion_nosigns_issue_ppml_uniform_$(MAIN_BW_LABEL)_geo_wardpair.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript did_table_permit_2015.R low_discretion_nosigns $(MAIN_BW_M) uniform block "$@"

../output/did_table_permit_2015_low_discretion_nosigns_issue_ppml_uniform_$(MAIN_BW_LABEL)_geo_wardpair_minsegment500ft.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	MIN_SEGMENT_LENGTH_FT=500 Rscript did_table_permit_2015.R low_discretion_nosigns $(MAIN_BW_M) uniform block "$@"

../output/did_table_permit_2015_low_discretion_nosigns_issue_ppml_uniform_$(MAIN_BW_LABEL)_geo_wardpair_minsegment1000ft.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	MIN_SEGMENT_LENGTH_FT=1000 Rscript did_table_permit_2015.R low_discretion_nosigns $(MAIN_BW_M) uniform block "$@"

../input/permit_block_year_panel.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel.parquet ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv

include ../../shell_functions.make
include ../../generic.make
