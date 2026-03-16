SHELL := bash
include ../../shell_functions.make

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
	../output/segment_feature_distance_audit_1320.csv \
	../output/pruned_boundaries_uncertainty_priority.csv \
	../output/pruned_boundaries_audit_pair_era.csv \
	../output/pruned_boundaries_audit_borderline_drop.csv \
	../output/pruned_boundaries_audit_unchecked_pair_era.csv \
	../output/pruned_boundaries_audit_extra_flag_pair_era.csv \
	../output/pruned_boundaries_audit.md

INPUTS := \
	../input/boundary_segments_1320ft.gpkg \
	../input/ward_panel.gpkg \
	../input/confounded_pair_era_flags.csv \
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

$(OUTPUTS): plot_pruned_boundary_maps.R $(INPUTS) | ../output
	Rscript plot_pruned_boundary_maps.R

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../border_segment_creation/input/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg: ../../border_segment_creation/input/gis_osm_water_a_free_1.cpg | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.dbf: ../../border_segment_creation/input/gis_osm_water_a_free_1.dbf | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.prj: ../../border_segment_creation/input/gis_osm_water_a_free_1.prj | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../border_segment_creation/input/gis_osm_water_a_free_1.shp | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shx: ../../border_segment_creation/input/gis_osm_water_a_free_1.shx | ../input
	ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.cpg: ../../border_segment_creation/input/gis_osm_landuse_a_free_1.cpg | ../input
	ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.dbf: ../../border_segment_creation/input/gis_osm_landuse_a_free_1.dbf | ../input
	ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.prj: ../../border_segment_creation/input/gis_osm_landuse_a_free_1.prj | ../input
	ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.shp: ../../border_segment_creation/input/gis_osm_landuse_a_free_1.shp | ../input
	ln -sf "$<" "$@"

../input/gis_osm_landuse_a_free_1.shx: ../../border_segment_creation/input/gis_osm_landuse_a_free_1.shx | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUTS)

include ../../generic.make
