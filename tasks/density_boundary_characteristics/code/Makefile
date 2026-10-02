include ../../shared/code/shell_functions.make

OSM_LAYERS = roads_free_1 landuse_a_free_1 water_a_free_1 waterways_free_1
OSM_FILES = $(foreach layer,$(OSM_LAYERS),$(foreach ext,shp dbf prj shx cpg,../input/gis_osm_$(layer).$(ext)))
WATER_FILES = $(foreach ext,shp dbf prj shx cpg,../input/gis_osm_water_a_free_1.$(ext))

all: ../output/boundary_feature_measurements.csv ../output/density_boundary_characteristics.csv

../output/density_boundary_characteristics.csv: build_density_boundary_characteristics.R ../output/boundary_feature_measurements.csv \
	../input/new_construction_analysis_data.csv ../input/ward_pair_boundaries.gpkg ../input/schools_2015.gpkg ../input/parks.gpkg \
	$(WATER_FILES) | ../output ../report
	$(R) $<

../output/boundary_feature_measurements.csv: build_boundary_features.R ../input/boundary_segments_1320ft.gpkg \
	../input/major_streets.geojson $(OSM_FILES) | ../output ../report
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
$(OSM_FILES): ../input/%: ../../../data_raw/illinois-250919-free/% | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv ../input/boundary_segments_1320ft.gpkg ../input/ward_pair_boundaries.gpkg \
	../input/major_streets.geojson ../input/schools_2015.gpkg ../input/parks.gpkg $(OSM_FILES)

include ../../shared/code/generic.make
