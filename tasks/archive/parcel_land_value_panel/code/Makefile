SHELL := bash
include ../../shell_functions.make

all: ../output/parcel_land_value_panel.parquet

../output/parcel_land_value_panel.parquet: build_parcel_land_value_panel.R ../input/assessor_history ../input/assessor_current ../input/parcels_current.csv | ../output
	$(R) build_parcel_land_value_panel.R ../input/assessor_history ../input/assessor_current ../input/parcels_current.csv ../output/parcel_land_value_panel.parquet

../input/assessor_history: ../../../data_raw/assessor_history | ../input
	ln -sfn $< $@

../input/assessor_current: ../../../data_raw/assessor | ../input
	ln -sfn $< $@

../input/parcels_current.csv: ../../../data_raw/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/assessor_history ../input/assessor_current ../input/parcels_current.csv
