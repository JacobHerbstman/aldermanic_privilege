SHELL := bash

SEGMENT_LENGTH_FT ?= 1320
NEAR_FEATURE_DISTANCE_M ?= 75

all: ../output/pruned_boundaries_audit.md

../output/pruned_boundaries_audit.md: plot_pruned_boundary_maps_audit.R \
	../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg \
	../input/ward_panel.gpkg \
	../input/confounded_pair_era_flags.csv \
	../input/confounded_segment_flags.csv \
	../input/parcels_with_ward_distances.csv \
	../input/gis_osm_water_a_free_1.cpg \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_landuse_a_free_1.cpg \
	../input/gis_osm_landuse_a_free_1.dbf \
	../input/gis_osm_landuse_a_free_1.prj \
	../input/gis_osm_landuse_a_free_1.shp \
	../input/gis_osm_landuse_a_free_1.shx | ../output
	SEGMENT_LENGTH_FT=$(SEGMENT_LENGTH_FT) NEAR_FEATURE_DISTANCE_M=$(NEAR_FEATURE_DISTANCE_M) Rscript plot_pruned_boundary_maps_audit.R

../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg: ../../../border_segment_creation/output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/confounded_segment_flags.csv: ../../../border_confound_pruning/output/confounded_segment_flags.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.shx: ../input/gis_osm_water_a_free_1.%: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.cpg ../input/gis_osm_landuse_a_free_1.dbf ../input/gis_osm_landuse_a_free_1.prj ../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_landuse_a_free_1.shx: ../input/gis_osm_landuse_a_free_1.%: ../../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg \
	../input/ward_panel.gpkg \
	../input/confounded_pair_era_flags.csv \
	../input/confounded_segment_flags.csv \
	../input/parcels_with_ward_distances.csv \
	../input/gis_osm_water_a_free_1.cpg \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_landuse_a_free_1.cpg \
	../input/gis_osm_landuse_a_free_1.dbf \
	../input/gis_osm_landuse_a_free_1.prj \
	../input/gis_osm_landuse_a_free_1.shp \
	../input/gis_osm_landuse_a_free_1.shx

include ../../../shell_functions.make
include ../../../generic.make
