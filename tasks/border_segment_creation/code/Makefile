SHELL := bash
include ../../shell_functions.make

REBUILD_MODE ?= raw
SEGMENT_LENGTHS_M ?= 400 800
SEGMENT_LAYER_BWS_M ?= 100 250 400
SEGMENT_FEATURE_BUFFER_M ?= 30
PRIMARY_SEGMENT_M := $(word 1,$(SEGMENT_LENGTHS_M))
SECONDARY_SEGMENT_M := $(word 2,$(SEGMENT_LENGTHS_M))

all: \
	../output/boundary_segments_$(PRIMARY_SEGMENT_M)m.gpkg \
	../output/boundary_segments_$(SECONDARY_SEGMENT_M)m.gpkg \
	../output/segment_classification.csv \
	../output/ward_pair_boundaries.gpkg \
	../output/ward_pair_boundary_summary.csv \
	../output/ward_pair_boundary_diagnostics.csv \
	../output/segment_feature_source_audit.csv

../output/boundary_segments_$(PRIMARY_SEGMENT_M)m.gpkg: build_boundary_segments.R ../../_lib/canonical_geometry_helpers.R ../input/ward_panel.gpkg ../input/canonical_boundary_segments_400m.gpkg ../input/canonical_boundary_segments_800m.gpkg ../input/canonical_segment_classification.csv ../input/canonical_ward_pair_boundaries.gpkg ../input/Major_Streets.shp ../input/Major_Streets.dbf ../input/Major_Streets.prj ../input/Major_Streets.shx ../input/gis_osm_roads_free_1.shp ../input/gis_osm_roads_free_1.dbf ../input/gis_osm_roads_free_1.prj ../input/gis_osm_roads_free_1.shx ../input/gis_osm_roads_free_1.cpg ../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_landuse_a_free_1.dbf ../input/gis_osm_landuse_a_free_1.prj ../input/gis_osm_landuse_a_free_1.shx ../input/gis_osm_landuse_a_free_1.cpg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg ../input/gis_osm_waterways_free_1.shp ../input/gis_osm_waterways_free_1.dbf ../input/gis_osm_waterways_free_1.prj ../input/gis_osm_waterways_free_1.shx ../input/gis_osm_waterways_free_1.cpg | ../output
	REBUILD_MODE=$(REBUILD_MODE) SEGMENT_LENGTHS_M="$(SEGMENT_LENGTHS_M)" SEGMENT_LAYER_BWS_M="$(SEGMENT_LAYER_BWS_M)" SEGMENT_FEATURE_BUFFER_M="$(SEGMENT_FEATURE_BUFFER_M)" Rscript build_boundary_segments.R

../output/boundary_segments_$(SECONDARY_SEGMENT_M)m.gpkg \
../output/segment_classification.csv \
../output/ward_pair_boundaries.gpkg \
../output/ward_pair_boundary_summary.csv \
../output/ward_pair_boundary_diagnostics.csv \
../output/segment_feature_source_audit.csv: ../output/boundary_segments_$(PRIMARY_SEGMENT_M)m.gpkg
	test -f $@

../input/canonical_boundary_segments_400m.gpkg: ../../../data_raw/boundaries/segments/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/canonical_boundary_segments_800m.gpkg: ../../../data_raw/boundaries/segments/boundary_segments_2640ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/canonical_segment_classification.csv: ../../../data_raw/boundaries/segments/segment_classification.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/canonical_ward_pair_boundaries.gpkg: ../../../data_raw/boundaries/segments/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/Major_Streets.shp ../input/Major_Streets.dbf ../input/Major_Streets.prj ../input/Major_Streets.shx: ../input/Major_Streets.%: ../../../data_raw/Major_Streets.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.cpg ../input/gis_osm_roads_free_1.dbf ../input/gis_osm_roads_free_1.prj ../input/gis_osm_roads_free_1.shp ../input/gis_osm_roads_free_1.shx: ../input/gis_osm_roads_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.cpg ../input/gis_osm_landuse_a_free_1.dbf ../input/gis_osm_landuse_a_free_1.prj ../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_landuse_a_free_1.shx: ../input/gis_osm_landuse_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.shx: ../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_waterways_free_1.cpg ../input/gis_osm_waterways_free_1.dbf ../input/gis_osm_waterways_free_1.prj ../input/gis_osm_waterways_free_1.shp ../input/gis_osm_waterways_free_1.shx: ../input/gis_osm_waterways_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/ward_panel.gpkg ../input/canonical_boundary_segments_400m.gpkg ../input/canonical_boundary_segments_800m.gpkg ../input/canonical_segment_classification.csv ../input/canonical_ward_pair_boundaries.gpkg ../input/Major_Streets.shp ../input/Major_Streets.dbf ../input/Major_Streets.prj ../input/Major_Streets.shx ../input/gis_osm_roads_free_1.shp ../input/gis_osm_roads_free_1.dbf ../input/gis_osm_roads_free_1.prj ../input/gis_osm_roads_free_1.shx ../input/gis_osm_roads_free_1.cpg ../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_landuse_a_free_1.dbf ../input/gis_osm_landuse_a_free_1.prj ../input/gis_osm_landuse_a_free_1.shx ../input/gis_osm_landuse_a_free_1.cpg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg ../input/gis_osm_waterways_free_1.shp ../input/gis_osm_waterways_free_1.dbf ../input/gis_osm_waterways_free_1.prj ../input/gis_osm_waterways_free_1.shx ../input/gis_osm_waterways_free_1.cpg

include ../../generic.make
