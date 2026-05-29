SEGMENT_LENGTH_FT ?= 1320
SEGMENT_LAYER_BWS_M ?= 100 250 400
SEGMENT_FEATURE_BUFFER_M ?= 30

all: \
	../output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg \
	../output/segment_classification.csv \
	../output/ward_pair_boundaries.gpkg

../output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg: build_boundary_segments.R \
	../../_lib/canonical_geometry_helpers.R \
	../input/ward_panel.gpkg \
	../input/Major_Streets.shp \
	../input/Major_Streets.dbf \
	../input/Major_Streets.prj \
	../input/Major_Streets.shx \
	../input/gis_osm_roads_free_1.shp \
	../input/gis_osm_roads_free_1.dbf \
	../input/gis_osm_roads_free_1.prj \
	../input/gis_osm_roads_free_1.shx \
	../input/gis_osm_roads_free_1.cpg \
	../input/gis_osm_landuse_a_free_1.shp \
	../input/gis_osm_landuse_a_free_1.dbf \
	../input/gis_osm_landuse_a_free_1.prj \
	../input/gis_osm_landuse_a_free_1.shx \
	../input/gis_osm_landuse_a_free_1.cpg \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg \
	../input/gis_osm_waterways_free_1.shp \
	../input/gis_osm_waterways_free_1.dbf \
	../input/gis_osm_waterways_free_1.prj \
	../input/gis_osm_waterways_free_1.shx \
	../input/gis_osm_waterways_free_1.cpg | ../output
	SEGMENT_LENGTH_FT=$(SEGMENT_LENGTH_FT) SEGMENT_LAYER_BWS_M="$(SEGMENT_LAYER_BWS_M)" SEGMENT_FEATURE_BUFFER_M="$(SEGMENT_FEATURE_BUFFER_M)" Rscript build_boundary_segments.R

../output/segment_classification.csv \
../output/ward_pair_boundaries.gpkg: ../output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg
	@test -s "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/Major_Streets.shp ../input/Major_Streets.dbf ../input/Major_Streets.prj ../input/Major_Streets.shx: ../input/Major_Streets.%: ../../../data_raw/Major_Streets.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.cpg ../input/gis_osm_roads_free_1.dbf ../input/gis_osm_roads_free_1.prj ../input/gis_osm_roads_free_1.shp ../input/gis_osm_roads_free_1.shx: ../input/gis_osm_roads_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.cpg ../input/gis_osm_landuse_a_free_1.dbf ../input/gis_osm_landuse_a_free_1.prj ../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_landuse_a_free_1.shx: ../input/gis_osm_landuse_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.shx: ../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_waterways_free_1.cpg ../input/gis_osm_waterways_free_1.dbf ../input/gis_osm_waterways_free_1.prj ../input/gis_osm_waterways_free_1.shp ../input/gis_osm_waterways_free_1.shx: ../input/gis_osm_waterways_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/ward_panel.gpkg \
	../input/Major_Streets.shp \
	../input/Major_Streets.dbf \
	../input/Major_Streets.prj \
	../input/Major_Streets.shx \
	../input/gis_osm_roads_free_1.shp \
	../input/gis_osm_roads_free_1.dbf \
	../input/gis_osm_roads_free_1.prj \
	../input/gis_osm_roads_free_1.shx \
	../input/gis_osm_roads_free_1.cpg \
	../input/gis_osm_landuse_a_free_1.shp \
	../input/gis_osm_landuse_a_free_1.dbf \
	../input/gis_osm_landuse_a_free_1.prj \
	../input/gis_osm_landuse_a_free_1.shx \
	../input/gis_osm_landuse_a_free_1.cpg \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg \
	../input/gis_osm_waterways_free_1.shp \
	../input/gis_osm_waterways_free_1.dbf \
	../input/gis_osm_waterways_free_1.prj \
	../input/gis_osm_waterways_free_1.shx \
	../input/gis_osm_waterways_free_1.cpg

include ../../generic.make
