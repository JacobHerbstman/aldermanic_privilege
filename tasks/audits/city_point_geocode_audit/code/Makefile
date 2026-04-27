SHELL := bash
include ../../../shell_functions.make

all: ../output/geocode_audit_report.md

../output/geocode_audit_report.md: run_city_point_geocode_audit.R ../input/building_permits_raw.csv ../input/building_permits_clean.gpkg ../input/affordable_rental_housing.csv ../input/cpd_facilities.geojson ../input/cps_school_locations.geojson ../input/city_boundary.geojson ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv | ../output
	$(R) run_city_point_geocode_audit.R

../input/building_permits_raw.csv: ../../../../data_raw/Building_Permits_20251121.csv | ../input
	ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	ln -sf "$<" "$@"

../input/affordable_rental_housing.csv: ../../../../data_raw/Affordable_Rental_Housing_Developments_20251121.csv | ../input
	ln -sf "$<" "$@"

../input/cpd_facilities.geojson: ../../../../data_raw/CPD_Facilities_20250925.geojson | ../input
	ln -sf "$<" "$@"

../input/cps_school_locations.geojson: ../../../../data_raw/CPS_School_Locations_SY1415_20250925.geojson | ../input
	ln -sf "$<" "$@"

../input/city_boundary.geojson: ../../../../data_raw/Boundaries_-_City_20250920.geojson | ../input
	ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf "$<" "$@"

../input/census_blocks_2020.csv: ../../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	ln -sf "$<" "$@"

include ../../../generic.make

link-inputs: ../input/building_permits_raw.csv ../input/building_permits_clean.gpkg ../input/affordable_rental_housing.csv ../input/cpd_facilities.geojson ../input/cps_school_locations.geojson ../input/city_boundary.geojson ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv
