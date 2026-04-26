SHELL := bash
include ../../shell_functions.make

all: ../output/block_land_value_panel.parquet ../output/block_land_value_panel_support.csv

../output/block_land_value_panel.parquet: build_block_land_value_panel.R ../input/parcel_land_redistricting_panel.parquet | ../output
	$(R) build_block_land_value_panel.R ../input/parcel_land_redistricting_panel.parquet ../output/block_land_value_panel.parquet ../output/block_land_value_panel_support.csv

../output/block_land_value_panel_support.csv: ../output/block_land_value_panel.parquet
	test -f $@

../input/parcel_land_redistricting_panel.parquet: ../../create_parcel_land_redistricting_dataset/output/parcel_land_redistricting_panel.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcel_land_redistricting_panel.parquet
