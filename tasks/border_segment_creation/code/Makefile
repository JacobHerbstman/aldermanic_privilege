SHELL := bash
include ../../shell_functions.make

all: \
	../output/boundary_segments_1320ft.gpkg \
	../output/boundary_segments_2640ft.gpkg \
	../output/segment_classification.csv \
	../output/ward_pair_boundaries.gpkg \
	../output/ward_pair_boundary_summary.csv \
	../output/ward_pair_boundary_diagnostics.csv

../output/boundary_segments_1320ft.gpkg: build_boundary_segments.R ../../_lib/canonical_geometry_helpers.R ../input/ward_panel.gpkg ../input/Major_Streets.shp ../input/Major_Streets.dbf ../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_waterways_free_1.shp | ../output
	# optional env: REBUILD_MODE=reuse|raw (default raw)
	Rscript build_boundary_segments.R

../output/boundary_segments_2640ft.gpkg \
../output/segment_classification.csv \
../output/ward_pair_boundaries.gpkg \
../output/ward_pair_boundary_summary.csv \
../output/ward_pair_boundary_diagnostics.csv: ../output/boundary_segments_1320ft.gpkg
	test -f $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/Major_Streets.shp: ../../../data_raw/Major_Streets.shp | ../input
	ln -sf "$<" "$@"

../input/Major_Streets.dbf: ../../../data_raw/Major_Streets.dbf | ../input
	ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.shp | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	ln -sf "$<" "$@"

../input/gis_osm_waterways_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_waterways_free_1.shp | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/ward_panel.gpkg ../input/Major_Streets.shp ../input/Major_Streets.dbf ../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_waterways_free_1.shp

include ../../generic.make
