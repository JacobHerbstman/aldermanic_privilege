SHELL := bash
include ../../shell_functions.make

BALANCE_BW_M ?= 100
BALANCE_BW_LABEL ?= $(BALANCE_BW_M)m
SAMPLE ?= all

all: ../output/density_amenity_balance_$(BALANCE_BW_LABEL)_$(SAMPLE).tex

../output/density_amenity_balance_$(BALANCE_BW_LABEL)_$(SAMPLE).csv ../output/density_amenity_balance_$(BALANCE_BW_LABEL)_$(SAMPLE).tex: build_density_amenity_balance.R ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript build_density_amenity_balance.R ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../output/density_amenity_balance_$(BALANCE_BW_LABEL)_$(SAMPLE).csv ../output/density_amenity_balance_$(BALANCE_BW_LABEL)_$(SAMPLE).tex $(BALANCE_BW_M) $(SAMPLE) $(BALANCE_BW_LABEL)

../input/parcels_with_geometry.gpkg: ../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../download_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../download_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../download_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg: ../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../generic.make
