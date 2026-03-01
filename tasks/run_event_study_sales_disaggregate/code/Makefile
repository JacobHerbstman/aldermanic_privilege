SHELL := bash
include ../../shell_functions.make

SCRIPT := run_event_study.R
DID_SCRIPT := run_did_sales_disaggregate.R

ALL_INPUTS := ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet

ES_CONT := ../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft_no_hedonics.pdf
ES_SPLIT := ../output/event_study_disaggregate_yearly_stacked_announcement_continuous_split_uniform_1000ft_no_hedonics.pdf
DID_CLEAN_TARGET := ../output/did_table_sales_clean_uniform_1000ft.tex
COHORT ?= 2015

.DEFAULT_GOAL := all
.PHONY: all link-inputs segment-robustness

all: $(ES_CONT) $(ES_SPLIT) $(DID_CLEAN_TARGET)

$(ES_CONT): $(SCRIPT) $(ALL_INPUTS) | ../output
	# arg order: treatment_type include_hedonics time_unit fe_type weighting bandwidth stacked stack_type cohort post_window geo_fe_level cluster_level
	Rscript $(SCRIPT) continuous FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full segment twoway_block_segment

$(ES_SPLIT): $(SCRIPT) $(ALL_INPUTS) | ../output
	# arg order: treatment_type include_hedonics time_unit fe_type weighting bandwidth stacked stack_type cohort post_window geo_fe_level cluster_level
	Rscript $(SCRIPT) continuous_split FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full segment twoway_block_segment

$(DID_CLEAN_TARGET): $(DID_SCRIPT) ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet | ../output
	# arg order: bandwidth weighting fe_type geo_fe_level cluster_level
	Rscript $(DID_SCRIPT) 1000 uniform strict_pair_x_year segment twoway_block_segment
	cp ../output/did_table_sales_clean.tex $@

ROBUST_ES_OUTPUTS := \
	../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft_no_hedonics_geo_wardpair.pdf \
	../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft_no_hedonics_clust_block.pdf \
	../output/event_study_disaggregate_yearly_stacked_announcement_continuous_split_uniform_1000ft_no_hedonics_geo_wardpair.pdf \
	../output/event_study_disaggregate_yearly_stacked_announcement_continuous_split_uniform_1000ft_no_hedonics_clust_block.pdf \
	../output/did_table_sales_clean_geo_wardpair.tex \
	../output/did_table_sales_clean_clust_block.tex

segment-robustness: $(ROBUST_ES_OUTPUTS)

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft_no_hedonics_geo_wardpair.pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) continuous FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full ward_pair twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft_no_hedonics_clust_block.pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) continuous FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full segment block

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_split_uniform_1000ft_no_hedonics_geo_wardpair.pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) continuous_split FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full ward_pair twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_split_uniform_1000ft_no_hedonics_clust_block.pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) continuous_split FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full segment block

../output/did_table_sales_clean_geo_wardpair.tex: $(DID_SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(DID_SCRIPT) 1000 uniform strict_pair_x_year ward_pair twoway_block_segment

../output/did_table_sales_clean_clust_block.tex: $(DID_SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(DID_SCRIPT) 1000 uniform strict_pair_x_year segment block

../input/sales_transaction_panel.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_announcement.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_announcement.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2012.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2022.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2022.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2023.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2023.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(ALL_INPUTS)

include ../../generic.make
