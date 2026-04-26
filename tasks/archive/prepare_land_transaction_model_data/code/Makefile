SHELL := bash
include ../../shell_functions.make

all: ../output/land_transaction_incidence_model_panel.parquet ../output/land_transaction_price_model_panel.parquet ../output/land_transaction_model_sample_support.csv

../output/land_transaction_incidence_model_panel.parquet ../output/land_transaction_price_model_panel.parquet ../output/land_transaction_model_sample_support.csv: prepare_land_transaction_model_data.R ../input/land_transaction_sale_events.parquet ../input/land_transaction_incidence_panel.parquet | ../output
	$(R) prepare_land_transaction_model_data.R ../input/land_transaction_sale_events.parquet ../input/land_transaction_incidence_panel.parquet ../output/land_transaction_incidence_model_panel.parquet ../output/land_transaction_price_model_panel.parquet ../output/land_transaction_model_sample_support.csv

../input/land_transaction_sale_events.parquet: ../../build_land_transaction_event_data/output/land_transaction_sale_events.parquet | ../input
	ln -sf $< $@

../input/land_transaction_incidence_panel.parquet: ../../build_land_transaction_incidence_panel/output/land_transaction_incidence_panel.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/land_transaction_sale_events.parquet ../input/land_transaction_incidence_panel.parquet
