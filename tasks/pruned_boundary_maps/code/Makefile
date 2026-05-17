SHELL := bash

SEGMENT_LENGTH_FT ?= 1320
NEAR_FEATURE_DISTANCE_M ?= 75

OUTPUTS := \
	../output/pruned_boundaries_all_segments_by_era.pdf \
	../output/pruned_boundaries_kept_segments_by_era.pdf \
	../output/pruned_boundaries_status_by_era.pdf \
	../output/pruned_boundaries_kept_strictness_gap_by_era.pdf \
	../output/pruned_boundaries_drop_reason_by_era.pdf \
	../output/pruned_boundaries_zoom_lakeview_status.pdf \
	../output/pruned_boundaries_zoom_lakeview_drop_reason.pdf \
	../output/pruned_boundaries_zoom_downtown_status.pdf \
	../output/pruned_boundaries_zoom_downtown_drop_reason.pdf \
	../output/pruned_boundaries_map_summary.csv \
	../output/pruned_boundaries_zoom_lakeview_pairs.csv \
	../output/pruned_boundaries_zoom_downtown_pairs.csv \
	../output/pruned_boundaries_zoom_lakeview_feature_context.csv \
	../output/pruned_boundaries_zoom_downtown_feature_context.csv \
	../output/segment_feature_distance_audit_$(SEGMENT_LENGTH_FT)ft.csv \
	../output/pruned_boundaries_uncertainty_priority.csv \
	../output/pruned_boundaries_audit_pair_era.csv \
	../output/pruned_boundaries_audit_borderline_drop.csv \
	../output/pruned_boundaries_audit_unchecked_pair_era.csv \
	../output/pruned_boundaries_audit_extra_flag_pair_era.csv \
	../output/pruned_boundaries_audit.md

PRIMARY_OUTPUT := ../output/pruned_boundaries_all_segments_by_era.pdf
SECONDARY_OUTPUTS := $(filter-out $(PRIMARY_OUTPUT),$(OUTPUTS))

INPUTS := \
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

all: $(OUTPUTS)

$(PRIMARY_OUTPUT): plot_pruned_boundary_maps.R $(INPUTS) | ../output
	SEGMENT_LENGTH_FT=$(SEGMENT_LENGTH_FT) NEAR_FEATURE_DISTANCE_M=$(NEAR_FEATURE_DISTANCE_M) Rscript plot_pruned_boundary_maps.R

$(SECONDARY_OUTPUTS): $(PRIMARY_OUTPUT) | ../output
	@test -s "$@"

../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg: ../../border_segment_creation/output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/confounded_segment_flags.csv: ../../border_confound_pruning/output/confounded_segment_flags.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.shx: ../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.cpg ../input/gis_osm_landuse_a_free_1.dbf ../input/gis_osm_landuse_a_free_1.prj ../input/gis_osm_landuse_a_free_1.shp ../input/gis_osm_landuse_a_free_1.shx: ../input/gis_osm_landuse_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_landuse_a_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: $(INPUTS)

include ../../shell_functions.make
include ../../generic.make
