SHELL := bash
include ../../shell_functions.make

all: ../output/land_transaction_sale_events.parquet ../output/land_transaction_diagnostics.csv

../output/land_transaction_sale_events.parquet: build_land_transaction_event_data.R ../input/land_transaction_sales_pin10_events.parquet ../input/parcel_land_redistricting_panel.parquet | ../output
	$(R) build_land_transaction_event_data.R ../input/land_transaction_sales_pin10_events.parquet ../input/parcel_land_redistricting_panel.parquet ../output/land_transaction_sale_events.parquet ../output/land_transaction_diagnostics.csv

../output/land_transaction_diagnostics.csv: ../output/land_transaction_sale_events.parquet
	test -f $@

../input/land_transaction_sales_pin10_events.parquet: ../../prepare_land_transaction_sales/output/land_transaction_sales_pin10_events.parquet | ../input
	ln -sf $< $@

../input/parcel_land_redistricting_panel.parquet: ../../create_parcel_land_redistricting_dataset/output/parcel_land_redistricting_panel.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/land_transaction_sales_pin10_events.parquet ../input/parcel_land_redistricting_panel.parquet
