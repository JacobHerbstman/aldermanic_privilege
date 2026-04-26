SHELL := bash
include ../../shell_functions.make

all: \
	../output/parcel_land_capitalization_prepost_2014_2016.parquet \
	../output/parcel_land_capitalization_prepost_2014_2016_support.csv \
	../output/parcel_land_capitalization_prepost_2011_2014.parquet \
	../output/parcel_land_capitalization_prepost_2011_2014_support.csv

../output/parcel_land_capitalization_prepost_2014_2016.parquet \
../output/parcel_land_capitalization_prepost_2014_2016_support.csv: build_parcel_land_capitalization_prepost_dataset.R ../input/parcel_land_redistricting_panel.parquet | ../output
	$(R) build_parcel_land_capitalization_prepost_dataset.R ../input/parcel_land_redistricting_panel.parquet ../output/parcel_land_capitalization_prepost_2014_2016.parquet ../output/parcel_land_capitalization_prepost_2014_2016_support.csv

../output/parcel_land_capitalization_prepost_2011_2014.parquet \
../output/parcel_land_capitalization_prepost_2011_2014_support.csv: build_parcel_land_capitalization_prepost_dataset.R ../input/parcel_land_redistricting_panel.parquet | ../output
	$(R) build_parcel_land_capitalization_prepost_dataset.R ../input/parcel_land_redistricting_panel.parquet ../output/parcel_land_capitalization_prepost_2011_2014.parquet ../output/parcel_land_capitalization_prepost_2011_2014_support.csv 2011 2014 2010 2011

../input/parcel_land_redistricting_panel.parquet: ../../create_parcel_land_redistricting_dataset/output/parcel_land_redistricting_panel.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcel_land_redistricting_panel.parquet
