SHELL := bash
include ../../shell_functions.make

BANDWIDTH ?= 1000

all: ../output/did_spec_scan_long.csv ../output/did_spec_scan_summary.csv

../output/.scan_done.stamp: run_did_spec_scan.R ../input/uncertainty_scores_output ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/rental_listing_panel.parquet | ../output
	$(R) run_did_spec_scan.R --scores_dir=../input/uncertainty_scores_output --block_treatment_pre=../input/block_treatment_pre_scores.csv --alderman_panel=../input/chicago_alderman_panel.csv --sales_2012=../input/sales_transaction_panel_2012.parquet --sales_2015=../input/sales_transaction_panel_2015.parquet --rental_panel=../input/rental_listing_panel.parquet --bandwidth=$(BANDWIDTH) --output_long=../output/did_spec_scan_long.csv --output_summary=../output/did_spec_scan_summary.csv
	touch $@

../output/did_spec_scan_long.csv: ../output/.scan_done.stamp
../output/did_spec_scan_summary.csv: ../output/.scan_done.stamp

../input/uncertainty_scores_output: ../../create_alderman_uncertainty_index/output | ../input
	ln -sfn $< $@

../input/block_treatment_pre_scores.csv: ../../create_block_treatment_panel/output/block_treatment_pre_scores.csv | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/sales_transaction_panel_2012.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet | ../input
	ln -sf $< $@

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf $< $@

../input/rental_listing_panel.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/uncertainty_scores_output ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/rental_listing_panel.parquet
