SHELL := bash
include ../../shell_functions.make

all: ../output/cta_stops.gpkg ../output/major_streets.gpkg ../output/parks.gpkg ../output/schools_2015.gpkg

../output/cta_stops.gpkg ../output/major_streets.gpkg ../output/parks.gpkg ../output/schools_2015.gpkg: get_amenities.R ../input/Major_Streets.shp ../input/cta_stations.geojson ../input/CPD_Facilities_20250925.geojson ../input/CPS_School_Locations_SY1415_20250925.geojson | ../output
	$(R) get_amenities.R

../input/Major_Streets.shp: ../../../data_raw/Major_Streets.shp | ../input
	ln -sf ../../../data_raw/Major_Streets.* $(dir $@)

../input/cta_stations.geojson: ../../../data_raw/cta_stations.geojson | ../input
	ln -sf $< $@

../input/CPD_Facilities_20250925.geojson: ../../../data_raw/CPD_Facilities_20250925.geojson | ../input
	ln -sf $< $@

../input/CPS_School_Locations_SY1415_20250925.geojson: ../../../data_raw/CPS_School_Locations_SY1415_20250925.geojson | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/Major_Streets.shp ../input/cta_stations.geojson ../input/CPD_Facilities_20250925.geojson ../input/CPS_School_Locations_SY1415_20250925.geojson
