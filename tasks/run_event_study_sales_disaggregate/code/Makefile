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
.PHONY: all link-inputs

all: $(ES_CONT) $(ES_SPLIT) $(DID_CLEAN_TARGET)

$(ES_CONT): $(SCRIPT) $(ALL_INPUTS) | ../output
	# arg order: treatment_type include_hedonics time_unit fe_type weighting bandwidth stacked stack_type cohort post_window
	Rscript $(SCRIPT) continuous FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full

$(ES_SPLIT): $(SCRIPT) $(ALL_INPUTS) | ../output
	# arg order: treatment_type include_hedonics time_unit fe_type weighting bandwidth stacked stack_type cohort post_window
	Rscript $(SCRIPT) continuous_split FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full

$(DID_CLEAN_TARGET): $(DID_SCRIPT) ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet | ../output
	# arg order: bandwidth weighting fe_type
	Rscript $(DID_SCRIPT) 1000 uniform strict_pair_x_year
	cp ../output/did_table_sales_clean.tex $@

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
