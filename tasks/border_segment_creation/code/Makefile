SHELL := bash
include ../../shell_functions.make

INPUTS := \
	../input/ward_panel.gpkg \
	../input/Major_Streets.shp \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_waterways_free_1.shp \
	../input/gis_osm_landuse_a_free_1.shp

OUTPUTS := \
	../output/ward_pair_boundaries.gpkg \
	../output/boundary_segments_1320ft.gpkg \
	../output/boundary_segments_660ft.gpkg \
	../output/boundary_segments_2640ft.gpkg \
	../output/segment_classification.csv \
	../output/assign_to_segment.R \
	../diagnostics/segment_diagnostics.md

all: ../output/.border_segment_creation.stamp

../output/.border_segment_creation.stamp: create_boundary_segments.R assign_to_segment.R $(INPUTS) | ../output ../diagnostics
	$(R) create_boundary_segments.R \
		--ward_panel=../input/ward_panel.gpkg \
		--major_streets=../input/Major_Streets.shp \
		--osm_water=../input/gis_osm_water_a_free_1.shp \
		--osm_waterways=../input/gis_osm_waterways_free_1.shp \
		--osm_landuse=../input/gis_osm_landuse_a_free_1.shp \
		--output_dir=../output \
		--diagnostics_dir=../diagnostics \
		--export_dir=../../../data_raw/boundaries/segments
	cp assign_to_segment.R ../output/assign_to_segment.R
	@for f in $(OUTPUTS); do \
		test -f $$f || (echo "Missing expected output: $$f" && exit 1); \
	done
	@touch $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/Major_Streets.shp: ../../../data_raw/Major_Streets.shp | ../input
	ln -sf ../../../data_raw/Major_Streets.* ../input/

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	ln -sf ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.* ../input/

../input/gis_osm_waterways_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.shp | ../input
	ln -sf ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.* ../input/

../input/gis_osm_landuse_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.shp | ../input
	ln -sf ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.* ../input/

../diagnostics:
	mkdir -p $@

link-inputs: $(INPUTS)

include ../../generic.make
