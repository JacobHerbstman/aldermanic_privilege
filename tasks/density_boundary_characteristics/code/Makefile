include ../../shared/code/shell_functions.make

all: ../output/boundary_feature_measurements.csv \
	../output/density_boundary_characteristics.csv

../output/boundary_feature_measurements.csv: build_boundary_features.R Makefile ../../setup_environment/code/packages.R ../input/boundary_segments_1320ft.gpkg ../input/major_streets.geojson ../input/gis_osm_roads_free_1.shp ../input/gis_osm_roads_free_1.dbf ../input/gis_osm_roads_free_1.prj ../input/gis_osm_roads_free_1.shx ../input/gis_osm_roads_free_1.cpg ../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_landuse_a_free_1.dbf ../input/gis_osm_landuse_a_free_1.prj ../input/gis_osm_landuse_a_free_1.shx ../input/gis_osm_landuse_a_free_1.cpg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg ../input/gis_osm_waterways_free_1.shp ../input/gis_osm_waterways_free_1.dbf ../input/gis_osm_waterways_free_1.prj ../input/gis_osm_waterways_free_1.shx ../input/gis_osm_waterways_free_1.cpg ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/density_boundary_characteristics.csv: build_density_boundary_characteristics.R Makefile ../../setup_environment/code/packages.R ../input/new_construction_analysis_data.csv ../input/ward_pair_boundaries.gpkg ../input/schools_2015.gpkg ../input/parks.gpkg ../output/boundary_feature_measurements.csv ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

../input/major_streets.geojson: ../../download_chicago_spatial_data/output/major_streets.geojson | ../input
	ln -sf $< $@

../input/schools_2015.gpkg: ../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	ln -sf $< $@

../input/parks.gpkg: ../../prepare_amenities_data/output/parks.gpkg | ../input
	ln -sf $< $@

../input/gis_osm_roads_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.shp | ../input
	ln -sf $< $@

../input/gis_osm_roads_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.dbf | ../input
	ln -sf $< $@

../input/gis_osm_roads_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.prj | ../input
	ln -sf $< $@

../input/gis_osm_roads_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.shx | ../input
	ln -sf $< $@

../input/gis_osm_roads_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.cpg | ../input
	ln -sf $< $@

../input/gis_osm_landuse_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.shp | ../input
	ln -sf $< $@

../input/gis_osm_landuse_a_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.dbf | ../input
	ln -sf $< $@

../input/gis_osm_landuse_a_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.prj | ../input
	ln -sf $< $@

../input/gis_osm_landuse_a_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.shx | ../input
	ln -sf $< $@

../input/gis_osm_landuse_a_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.cpg | ../input
	ln -sf $< $@

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	ln -sf $< $@

../input/gis_osm_water_a_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.dbf | ../input
	ln -sf $< $@

../input/gis_osm_water_a_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.prj | ../input
	ln -sf $< $@

../input/gis_osm_water_a_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shx | ../input
	ln -sf $< $@

../input/gis_osm_water_a_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.cpg | ../input
	ln -sf $< $@

../input/gis_osm_waterways_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.shp | ../input
	ln -sf $< $@

../input/gis_osm_waterways_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.dbf | ../input
	ln -sf $< $@

../input/gis_osm_waterways_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.prj | ../input
	ln -sf $< $@

../input/gis_osm_waterways_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.shx | ../input
	ln -sf $< $@

../input/gis_osm_waterways_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.cpg | ../input
	ln -sf $< $@

include ../../shared/code/generic.make
