SHELL := bash
include ../../shell_functions.make

UNCERTAINTY_SPEC := ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH
RD_FE_SPEC := zone_pair_year_additive

all: ../output/building_permits_clean_border500_filtered.gpkg \
	../output/high_discretion_boundary_filter_summary.csv \
	../output/high_discretion_boundary_distances.csv \
	../output/permits_for_uncertainty_index.csv \
	../output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv \
	../output/uncertainty_index_$(UNCERTAINTY_SPEC).pdf \
	../output/stage1_regression_$(UNCERTAINTY_SPEC).tex \
	../output/stage2_regression_$(UNCERTAINTY_SPEC).tex \
	../output/parcels_with_ward_distances.csv \
	../output/rd_fe_plot_log_density_far_bw500_all_$(RD_FE_SPEC)_permit_border500.pdf \
	../output/rd_fe_plot_log_density_dupac_bw500_all_$(RD_FE_SPEC)_permit_border500.pdf

../output/building_permits_clean_border500_filtered.gpkg ../output/high_discretion_boundary_filter_summary.csv ../output/high_discretion_boundary_distances.csv: ../output/.filter_border500.done

../output/.filter_border500.done: filter_high_discretion_permits_near_boundary.R ../input/building_permits_clean_raw.gpkg ../input/ward_panel.gpkg | ../output
	$(R) filter_high_discretion_permits_near_boundary.R
	touch "$@"

../output/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/code/prepare_uncertainty_data.R ../input/ward_panel.gpkg ../input/building_permits_clean.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/community_areas.geojson ../input/cta_stations.geojson ../input/city_boundary.geojson ../input/gis_osm_water_a_free_1.shp | ../output
	$(R) ../../data_for_alderman_uncertainty_index/code/prepare_uncertainty_data.R

../output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../output/uncertainty_index_$(UNCERTAINTY_SPEC).pdf ../output/stage1_regression_$(UNCERTAINTY_SPEC).tex ../output/stage2_regression_$(UNCERTAINTY_SPEC).tex: ../output/.uncertainty_border500.done

../output/.uncertainty_border500.done: ../../create_alderman_uncertainty_index/code/create_uncertainty_index.R ../input/permits_for_uncertainty_index.csv | ../output
	$(R) ../../create_alderman_uncertainty_index/code/create_uncertainty_index.R TRUE TRUE TRUE FALSE TRUE N_PERMITS LAG1 BOTH
	touch "$@"

../output/parcels_with_ward_distances.csv: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/aldermen_uncertainty_scores.csv | ../output
	$(R) ../../merge_in_scores/code/merge_scores.R ../input/aldermen_uncertainty_scores.csv uncertainty_index

../output/rd_fe_plot_log_density_far_bw500_all_$(RD_FE_SPEC)_permit_border500.pdf: ../../spatial_rd_fe/code/spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	$(R) ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 500 all $(RD_FE_SPEC) $@ slope

../output/rd_fe_plot_log_density_dupac_bw500_all_$(RD_FE_SPEC)_permit_border500.pdf: ../../spatial_rd_fe/code/spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	$(R) ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 500 all $(RD_FE_SPEC) $@ slope

../input/building_permits_clean_raw.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/building_permits_clean.gpkg: ../output/building_permits_clean_border500_filtered.gpkg | ../input
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

../input/permits_for_uncertainty_index.csv: ../output/permits_for_uncertainty_index.csv | ../input
	ln -sf $< $@

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf $< $@

../input/aldermen_uncertainty_scores.csv: ../output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	ln -sf $< $@

../input/parcels_with_ward_distances.csv: ../output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

link-inputs: ../input/building_permits_clean_raw.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/community_areas.geojson ../input/cta_stations.geojson ../input/city_boundary.geojson ../input/gis_osm_water_a_free_1.shp ../input/parcels_pre_scores.csv

include ../../generic.make
