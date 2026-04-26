SHELL := bash
include ../../shell_functions.make

all: \
	../output/parcel_land_year_source_panel_support.csv \
	../output/parcel_land_year_source_prepost_support.csv \
	../output/parcel_land_year_source_prepost_source_pairs.csv

../output/parcel_land_year_source_panel_support.csv \
../output/parcel_land_year_source_prepost_support.csv \
../output/parcel_land_year_source_prepost_source_pairs.csv: build_year_source_support_audit.R ../input/parcel_land_redistricting_panel.parquet ../input/parcel_land_capitalization_prepost_2014_2016.parquet | ../output
	$(R) build_year_source_support_audit.R ../input/parcel_land_redistricting_panel.parquet ../input/parcel_land_capitalization_prepost_2014_2016.parquet ../output/parcel_land_year_source_panel_support.csv ../output/parcel_land_year_source_prepost_support.csv ../output/parcel_land_year_source_prepost_source_pairs.csv

../input/parcel_land_redistricting_panel.parquet: ../../create_parcel_land_redistricting_dataset/output/parcel_land_redistricting_panel.parquet | ../input
	ln -sf $< $@

../input/parcel_land_capitalization_prepost_2014_2016.parquet: ../../prepare_parcel_land_capitalization_prepost_dataset/output/parcel_land_capitalization_prepost_2014_2016.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcel_land_redistricting_panel.parquet ../input/parcel_land_capitalization_prepost_2014_2016.parquet
