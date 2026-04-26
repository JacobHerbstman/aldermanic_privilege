SHELL := bash
include ../../shell_functions.make

all: ../output/land_transaction_incidence_panel.parquet ../output/land_transaction_support.csv

../output/land_transaction_incidence_panel.parquet: build_land_transaction_incidence_panel.R ../input/land_transaction_sale_events.parquet ../input/parcel_land_redistricting_panel.parquet | ../output
	$(R) build_land_transaction_incidence_panel.R ../input/land_transaction_sale_events.parquet ../input/parcel_land_redistricting_panel.parquet ../output/land_transaction_incidence_panel.parquet ../output/land_transaction_support.csv

../output/land_transaction_support.csv: ../output/land_transaction_incidence_panel.parquet
	test -f $@

../input/land_transaction_sale_events.parquet: ../../build_land_transaction_event_data/output/land_transaction_sale_events.parquet | ../input
	ln -sf $< $@

../input/parcel_land_redistricting_panel.parquet: ../../create_parcel_land_redistricting_dataset/output/parcel_land_redistricting_panel.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/land_transaction_sale_events.parquet ../input/parcel_land_redistricting_panel.parquet
