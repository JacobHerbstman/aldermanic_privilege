SHELL := bash
include ../../shell_functions.make

all: \
	../output/panelview_treat_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf \
	../output/panelview_missing_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf \
	../output/panelview_network_summary_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.csv \
	../output/panelview_support_crosswalk_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.csv \
	../output/panelview_treat_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf \
	../output/panelview_missing_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf \
	../output/panelview_network_summary_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.csv \
	../output/panelview_support_crosswalk_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.csv

../output/panelview_treat_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf: run_panelview_permit_diagnostics.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript run_panelview_permit_diagnostics.R cohort_2015 high_discretion issue ppml continuous uniform 1000 ward_pair none

../output/panelview_missing_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf \
../output/panelview_network_summary_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.csv \
../output/panelview_support_crosswalk_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.csv: ../output/panelview_treat_yearly_cohort_2015_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf

../output/panelview_treat_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf: run_panelview_permit_diagnostics.R ../input/permit_block_year_panel.parquet ../input/block_group_controls.csv | ../output
	Rscript run_panelview_permit_diagnostics.R stacked_implementation high_discretion issue ppml continuous uniform 1000 ward_pair none

../output/panelview_missing_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf \
../output/panelview_network_summary_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.csv \
../output/panelview_support_crosswalk_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.csv: ../output/panelview_treat_yearly_stacked_implementation_high_discretion_issue_ppml_continuous_uniform_1000ft_within_block_full_clust_block_geo_wardpair.pdf

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
