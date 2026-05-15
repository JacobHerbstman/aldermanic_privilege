SHELL := bash

all: ../output/permits_for_uncertainty_index.csv

../output/permits_for_uncertainty_index.csv: prepare_uncertainty_data.R \
	../input/ward_panel.gpkg \
	../input/building_permits_clean.gpkg \
	../input/chicago_alderman_panel.csv \
	../input/ward_controls.csv \
	../input/community_areas.geojson \
	../input/cta_stations.geojson \
	../input/city_boundary.geojson \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg | ../output
	$(R) $<

# Symlinks to upstream data
../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_controls.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/cta_stations.geojson: ../../../data_raw/cta_stations.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/city_boundary.geojson: ../../../data_raw/Boundaries_-_City_20250920.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg: ../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"


link-inputs: ../input/ward_panel.gpkg ../input/building_permits_clean.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/community_areas.geojson ../input/cta_stations.geojson ../input/city_boundary.geojson ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../shell_functions.make
include ../../generic.make
