include ../../shared/code/shell_functions.make

all: ../output/sales_with_hedonics_amenities.parquet

../output/sales_with_hedonics_amenities.parquet: enrich_sales_with_amenity_distances.R ../../shared/code/amenity_distance_helpers.R ../input/sales_with_hedonics.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg ../../setup_environment/code/packages.R Makefile ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../input/sales_with_hedonics.parquet: ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
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
	../input/sales_with_hedonics.parquet \
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
