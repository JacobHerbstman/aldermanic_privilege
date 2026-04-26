FOCAL_YVAR ?= density_dupac
MATCH_YVAR ?= density_far
SAMPLE ?= all
FE_SPEC ?= zonegroup_segment_year_additive
FOCAL_SHIFT_FT ?= -500
MIRROR_SHIFT_FT ?= 500
FOCAL_BW_FT ?= 250
BW_GRID ?= 100,150,200,250,300,400
TOP_K ?= 20
N_ZOOMS ?= 4
ZOOM_IDS := $(shell seq -f "%02g" 1 $(N_ZOOMS))

all: ../output/placebo_focal_check.csv \
	../output/placebo_bw_sweep.csv \
	../output/placebo_bin_profile.csv \
	../output/placebo_focal_parcel_audit.csv \
	../output/placebo_era_decomposition.csv \
	../output/placebo_pair_rankings.csv \
	../output/placebo_segment_rankings.csv \
	../output/placebo_composition_comparison.csv \
	../output/placebo_feature_context.csv \
	../output/placebo_zoning_street_summary.csv \
	../output/placebo_top_segments_citywide.pdf \
	../output/placebo_top10_zoning_street_context.pdf \
	../output/placebo_zoom_index.csv \
	../output/placebo_driver_audit.md \
	$(foreach zoom_id,$(ZOOM_IDS),../output/placebo_zoom_$(zoom_id).pdf)

../output/placebo_focal_check.csv ../output/placebo_bw_sweep.csv ../output/placebo_bin_profile.csv ../output/placebo_focal_parcel_audit.csv ../output/placebo_era_decomposition.csv ../output/placebo_pair_rankings.csv ../output/placebo_segment_rankings.csv ../output/placebo_composition_comparison.csv ../output/placebo_feature_context.csv ../output/placebo_zoning_street_summary.csv ../output/placebo_top_segments_citywide.pdf ../output/placebo_top10_zoning_street_context.pdf ../output/placebo_zoom_index.csv ../output/placebo_driver_audit.md $(foreach zoom_id,$(ZOOM_IDS),../output/placebo_zoom_$(zoom_id).pdf) &: audit_placebo_driver.R ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_1320ft.gpkg ../input/ward_panel.gpkg ../input/zoning_data_clean.gpkg ../input/gis_osm_roads_free_1.shp ../input/gis_osm_roads_free_1.dbf ../input/gis_osm_roads_free_1.shx ../input/gis_osm_roads_free_1.prj ../input/gis_osm_roads_free_1.cpg | ../output
	Rscript audit_placebo_driver.R ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_1320ft.gpkg ../input/ward_panel.gpkg ../input/zoning_data_clean.gpkg ../input/gis_osm_roads_free_1.shp $(FOCAL_YVAR) $(MATCH_YVAR) $(SAMPLE) $(FE_SPEC) $(FOCAL_SHIFT_FT) $(MIRROR_SHIFT_FT) $(FOCAL_BW_FT) "$(BW_GRID)" $(TOP_K) $(N_ZOOMS) ../output

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/parcels_with_geometry.gpkg: ../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../border_segment_creation/input/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/zoning_data_clean.gpkg: ../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
	ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.shp | ../input
	ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.dbf | ../input
	ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.shx | ../input
	ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.prj | ../input
	ln -sf "$<" "$@"

../input/gis_osm_roads_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_roads_free_1.cpg | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_1320ft.gpkg ../input/ward_panel.gpkg ../input/zoning_data_clean.gpkg ../input/gis_osm_roads_free_1.shp ../input/gis_osm_roads_free_1.dbf ../input/gis_osm_roads_free_1.shx ../input/gis_osm_roads_free_1.prj ../input/gis_osm_roads_free_1.cpg

include ../../generic.make
