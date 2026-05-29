SHELL := bash

MAIN_BW_M ?= 304.8
MAIN_BW_LABEL ?= 1000ft

all: \
	../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_coefficients_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_support_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_pretrend_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_metadata_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_coefficients_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_support_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_pretrend_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_metadata_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv \
	../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.csv \
	../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.csv \
	../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.csv \
	../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.csv \
	../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.csv \
	../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.csv \
	../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.csv \
	../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.csv

../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 high_discretion issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 high_discretion issue ppml continuous_split uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_coefficients_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< cohort_2015 low_discretion_nosigns issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_support_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_pretrend_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_metadata_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_cohort_2015_low_discretion_nosigns_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_coefficients_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel.parquet | ../output
	$(R) $< stacked_implementation high_discretion issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_support_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_pretrend_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_metadata_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv: | ../output/event_study_coefficients_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair.csv
	@test -s "$@"

../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	MIN_SEGMENT_LENGTH_FT=500 Rscript $< cohort_2015 high_discretion issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.csv
	@test -s "$@"

../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.csv
	@test -s "$@"

../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment500ft.csv
	@test -s "$@"

../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.csv: run_permit_event_study_audit.R ../../../_lib/event_study_plot_helpers.R ../../../_lib/permit_event_study_sample_helpers.R ../input/permit_block_year_panel_2015.parquet | ../output
	MIN_SEGMENT_LENGTH_FT=1000 Rscript $< cohort_2015 high_discretion issue ppml continuous uniform $(MAIN_BW_M) within_block full ward_pair block none none $(MAIN_BW_LABEL)

../output/event_study_support_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.csv
	@test -s "$@"

../output/event_study_pretrend_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.csv
	@test -s "$@"

../output/event_study_metadata_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.csv: | ../output/event_study_coefficients_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_$(MAIN_BW_LABEL)_within_block_full_clust_block_geo_wardpair_minsegment1000ft.csv
	@test -s "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/permit_block_year_panel.parquet

include ../../../shell_functions.make
include ../../../generic.make
