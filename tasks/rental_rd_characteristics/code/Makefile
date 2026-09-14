include ../../shared/code/shell_functions.make

BANDWIDTH_FT ?= 1500

all: ../output/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet

../output/rental_rd_characteristics_panel_bw%.parquet: build_rental_rd_characteristics.R ../../shared/code/amenity_distance_helpers.R ../input/rent_with_ward_distances.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg ../../setup_environment/code/packages.R Makefile ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $< $*

../input/rent_with_ward_distances.parquet: ../../merge_rent_scores/output/rent_with_ward_distances_full_through2022.parquet | ../input
	ln -sf $< $@

../input/schools_2015.gpkg: ../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	ln -sf $< $@

../input/parks.gpkg: ../../prepare_amenities_data/output/parks.gpkg | ../input
	ln -sf $< $@

../input/major_streets.gpkg: ../../prepare_amenities_data/output/major_streets.gpkg | ../input
	ln -sf $< $@

../input/cta_stops.gpkg: ../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	ln -sf $< $@

../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	ln -sf $< $@

link-inputs: \
	../input/rent_with_ward_distances.parquet \
	../input/schools_2015.gpkg \
	../input/parks.gpkg \
	../input/major_streets.gpkg \
	../input/cta_stops.gpkg \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg

include ../../shared/code/generic.make
