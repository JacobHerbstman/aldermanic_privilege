SHELL := bash
include ../../shell_functions.make

FILTER_SCRIPT := filter_high_discretion_permits_near_boundary.R
FILTERED_PERMITS := ../output/building_permits_clean_border500_filtered.gpkg
FILTER_SUMMARY := ../output/high_discretion_boundary_filter_summary.csv
FILTER_DETAIL := ../output/high_discretion_boundary_distances.csv
FILTER_STAMP := ../output/.filter_border500.done

PREP_SCRIPT := ../../data_for_alderman_uncertainty_index/code/prepare_uncertainty_data.R
PREP_OUTPUT := ../output/permits_for_uncertainty_index.csv

UNCERTAINTY_SCRIPT := ../../create_alderman_uncertainty_index/code/create_uncertainty_index.R
UNCERTAINTY_SPEC := ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH
UNCERTAINTY_CSV := ../output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv
UNCERTAINTY_PLOT := ../output/uncertainty_index_$(UNCERTAINTY_SPEC).pdf
UNCERTAINTY_STAGE1 := ../output/stage1_regression_$(UNCERTAINTY_SPEC).tex
UNCERTAINTY_STAGE2 := ../output/stage2_regression_$(UNCERTAINTY_SPEC).tex
UNCERTAINTY_STAMP := ../output/.uncertainty_border500.done

MERGE_SCRIPT := ../../merge_in_scores/code/merge_scores.R
MERGED_PARCELS := ../output/parcels_with_ward_distances.csv

SPATIAL_RD_FE_SCRIPT := ../../spatial_rd_fe/code/spatial_rd_fe.R
RD_FE_SPEC := zone_pair_year_additive
RD_FE_FAR_OUTPUT := ../output/rd_fe_plot_log_density_far_bw500_all_$(RD_FE_SPEC)_permit_border500.pdf
RD_FE_DUPAC_OUTPUT := ../output/rd_fe_plot_log_density_dupac_bw500_all_$(RD_FE_SPEC)_permit_border500.pdf

all: $(FILTERED_PERMITS) $(FILTER_SUMMARY) $(FILTER_DETAIL) $(PREP_OUTPUT) $(UNCERTAINTY_CSV) $(UNCERTAINTY_PLOT) $(UNCERTAINTY_STAGE1) $(UNCERTAINTY_STAGE2) $(MERGED_PARCELS) $(RD_FE_FAR_OUTPUT) $(RD_FE_DUPAC_OUTPUT)

$(FILTERED_PERMITS) $(FILTER_SUMMARY) $(FILTER_DETAIL): $(FILTER_STAMP)

$(FILTER_STAMP): $(FILTER_SCRIPT) ../input/building_permits_clean_raw.gpkg ../input/ward_panel.gpkg | ../output
	$(R) $(FILTER_SCRIPT)
	touch "$@"

$(PREP_OUTPUT): $(PREP_SCRIPT) ../input/ward_panel.gpkg ../input/building_permits_clean.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/community_areas.geojson ../input/cta_stations.geojson ../input/city_boundary.geojson ../input/gis_osm_water_a_free_1.shp | ../output
	$(R) $(PREP_SCRIPT)

$(UNCERTAINTY_CSV) $(UNCERTAINTY_PLOT) $(UNCERTAINTY_STAGE1) $(UNCERTAINTY_STAGE2): $(UNCERTAINTY_STAMP)

$(UNCERTAINTY_STAMP): $(UNCERTAINTY_SCRIPT) ../input/permits_for_uncertainty_index.csv | ../output
	$(R) $(UNCERTAINTY_SCRIPT) TRUE TRUE TRUE FALSE TRUE N_PERMITS LAG1 BOTH
	touch "$@"

$(MERGED_PARCELS): $(MERGE_SCRIPT) ../input/parcels_pre_scores.csv ../input/aldermen_uncertainty_scores.csv | ../output
	$(R) $(MERGE_SCRIPT) ../input/aldermen_uncertainty_scores.csv uncertainty_index

$(RD_FE_FAR_OUTPUT): $(SPATIAL_RD_FE_SCRIPT) ../input/parcels_with_ward_distances.csv | ../output
	$(R) $(SPATIAL_RD_FE_SCRIPT) density_far TRUE 500 all $(RD_FE_SPEC) $@ slope

$(RD_FE_DUPAC_OUTPUT): $(SPATIAL_RD_FE_SCRIPT) ../input/parcels_with_ward_distances.csv | ../output
	$(R) $(SPATIAL_RD_FE_SCRIPT) density_dupac TRUE 500 all $(RD_FE_SPEC) $@ slope

../input/building_permits_clean_raw.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/building_permits_clean.gpkg: $(FILTERED_PERMITS) | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/ward_controls.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	ln -sf $< $@

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	ln -sf $< $@

../input/cta_stations.geojson: ../../../data_raw/cta_stations.geojson | ../input
	ln -sf $< $@

../input/city_boundary.geojson: ../../../data_raw/Boundaries_-_City_20250920.geojson | ../input
	ln -sf $< $@

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	ln -sf ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.* $(dir $@)

../input/permits_for_uncertainty_index.csv: $(PREP_OUTPUT) | ../input
	ln -sf $< $@

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf $< $@

../input/aldermen_uncertainty_scores.csv: $(UNCERTAINTY_CSV) | ../input
	ln -sf $< $@

../input/parcels_with_ward_distances.csv: $(MERGED_PARCELS) | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/building_permits_clean_raw.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/community_areas.geojson ../input/cta_stations.geojson ../input/city_boundary.geojson ../input/gis_osm_water_a_free_1.shp ../input/parcels_pre_scores.csv
