SHELL := bash
include ../../shell_functions.make

# Build all four amenities files
all: ../output/cta_stops.gpkg ../output/major_streets.gpkg ../output/parks.gpkg ../output/schools_2015.gpkg

# All four outputs are produced by the same R script you showed
../output/cta_stops.gpkg ../output/major_streets.gpkg ../output/parks.gpkg ../output/schools_2015.gpkg: get_amenities.R | ../output link-inputs
	$(R) get_amenities.R

# Symlink raw data into ../input (directory + specific files your script references)
.PHONY: link-inputs
link-inputs: ../input
	ln -sf ../../../data_raw/Major_Streets.* ../input/
	ln -sf ../../../data_raw/cta_stations.geojson ../input/cta_stations.geojson
	ln -sf ../../../data_raw/CPD_Facilities_20250925.geojson ../input/CPD_Facilities_20250925.geojson
	ln -sf ../../../data_raw/CPS_School_Locations_SY1415_20250925.geojson ../input/CPS_School_Locations_SY1415_20250925.geojson
	@echo "Linked inputs into ../input"

include ../../generic.make
