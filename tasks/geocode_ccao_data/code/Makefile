SHELL := bash
include ../../shell_functions.make

all: ../output/geocoded_residential_data.gpkg

../output/geocoded_residential_data.gpkg: geocode_residential_data.R ../input/parcel_proximity.csv ../input/residential_cross_section.csv ../input/parcels.csv ../input/multifamily_data_cleaned.csv ../input/approved_commercial_density_blocker_corrections.csv | ../output
	$(R) geocode_residential_data.R ../input/approved_commercial_density_blocker_corrections.csv ../output/geocoded_residential_data.gpkg

../input/residential_cross_section.csv: ../../new_construction_source_fixes/output/residential_cross_section.csv | ../input
	ln -sf "$<" "$@"

../input/parcels.csv: ../../../data_raw/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv | ../input
	ln -sf "$<" "$@"

../input/parcel_proximity.csv: ../../../data_raw/Assessor_-_Parcel_Proximity_20251007.csv | ../input
	ln -sf "$<" "$@"

../input/multifamily_data_cleaned.csv: ../../new_construction_source_fixes/output/multifamily_data_cleaned.csv | ../input
	ln -sf "$<" "$@"

../input/approved_commercial_density_blocker_corrections.csv: ../../commercial_density_blocker_manual_corrections/output/approved_commercial_density_blocker_corrections.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/parcel_proximity.csv ../input/residential_cross_section.csv ../input/parcels.csv ../input/multifamily_data_cleaned.csv ../input/approved_commercial_density_blocker_corrections.csv
