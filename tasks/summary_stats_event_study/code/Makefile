SHELL := bash
include ../../shell_functions.make

all: ../output/summary_stats_event_study.tex

../output/summary_stats_event_study.tex: create_summary_stats.R ../input/sales_transaction_panel_2012.parquet ../input/rental_listing_panel.parquet | ../output
	Rscript create_summary_stats.R

../input/sales_transaction_panel_2012.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet | ../input
	ln -sf $< $@

../input/rental_listing_panel.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/sales_transaction_panel_2012.parquet ../input/rental_listing_panel.parquet
