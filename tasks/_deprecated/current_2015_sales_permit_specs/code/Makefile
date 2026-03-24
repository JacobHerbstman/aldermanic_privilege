SHELL := bash
include ../../shell_functions.make

BW_FT := 1000
WEIGHTING := uniform

all: \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_$(WEIGHTING)_$(BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf \
	../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_$(WEIGHTING)_$(BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf \
	../output/event_study_combined_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_$(WEIGHTING)_$(BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf \
	../output/event_study_disaggregate_yearly_cohort_2015_continuous_$(WEIGHTING)_$(BW_FT)ft_full_geo_wardpair_clust_block.pdf \
	../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_$(WEIGHTING)_$(BW_FT)ft_full_geo_wardpair_clust_block.pdf \
	../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_$(WEIGHTING)_$(BW_FT)ft_full_geo_wardpair_clust_block.pdf \
	../output/did_table_permit_2015_high_discretion_issue_ppml_$(WEIGHTING)_$(BW_FT)ft_geo_wardpair.tex \
	../output/did_table_sales_2015_$(WEIGHTING)_$(BW_FT)ft_geo_wardpair_clust_block.tex

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_$(WEIGHTING)_$(BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: ../../run_event_study_permit/code/run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript ../../run_event_study_permit/code/run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous $(WEIGHTING) $(BW_FT) within_block full ward_pair block none

../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_$(WEIGHTING)_$(BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: ../../run_event_study_permit/code/run_event_study_permit.R ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv | ../output
	Rscript ../../run_event_study_permit/code/run_event_study_permit.R cohort_2015 high_discretion issue ppml continuous_split $(WEIGHTING) $(BW_FT) within_block full ward_pair block none

../output/event_study_combined_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_$(WEIGHTING)_$(BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf: ../output/event_study_yearly_cohort_2015_high_discretion_issue_ppml_continuous_split_$(WEIGHTING)_$(BW_FT)ft_within_block_full_clust_block_geo_wardpair.pdf

../output/event_study_disaggregate_yearly_cohort_2015_continuous_$(WEIGHTING)_$(BW_FT)ft_full_geo_wardpair_clust_block.pdf: ../../run_event_study_sales_disaggregate/code/run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	WRITE_SIDECARS=0 Rscript ../../run_event_study_sales_disaggregate/code/run_event_study.R cohort_2015 continuous TRUE yearly strict_pair_x_year $(WEIGHTING) $(BW_FT) full ward_pair block

../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_$(WEIGHTING)_$(BW_FT)ft_full_geo_wardpair_clust_block.pdf: ../../run_event_study_sales_disaggregate/code/run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	WRITE_SIDECARS=0 Rscript ../../run_event_study_sales_disaggregate/code/run_event_study.R cohort_2015 continuous_split TRUE yearly strict_pair_x_year $(WEIGHTING) $(BW_FT) full ward_pair block

../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_$(WEIGHTING)_$(BW_FT)ft_full_geo_wardpair_clust_block.pdf: ../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_$(WEIGHTING)_$(BW_FT)ft_full_geo_wardpair_clust_block.pdf

../output/did_table_permit_2015_high_discretion_issue_ppml_$(WEIGHTING)_$(BW_FT)ft_geo_wardpair.tex: did_table_permit_2015.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript did_table_permit_2015.R $(BW_FT) $(WEIGHTING) $@

../output/did_table_sales_2015_$(WEIGHTING)_$(BW_FT)ft_geo_wardpair_clust_block.tex: did_table_sales_2015.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript did_table_sales_2015.R $(BW_FT) $(WEIGHTING) $@

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/block_group_controls.csv ../input/sales_transaction_panel_2015.parquet

include ../../generic.make
