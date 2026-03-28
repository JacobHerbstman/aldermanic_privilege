SHELL := bash
include ../../shell_functions.make

BASELINE_SPEC := ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH

all: \
	../output/building_permits_clean_border500_filtered.gpkg \
	../output/high_discretion_boundary_filter_summary.csv \
	../output/high_discretion_boundary_distances.csv \
	../output/permits_for_uncertainty_index_permit_border500.csv \
	../output/alderman_uncertainty_index_baseline.csv \
	../output/alderman_uncertainty_index_permit_border500.csv \
	../output/uncertainty_index_permit_border500.pdf \
	../output/stage1_regression_permit_border500.tex \
	../output/stage2_regression_permit_border500.tex \
	../output/parcels_with_ward_distances_baseline.csv \
	../output/boundary_distance_summary_baseline.csv \
	../output/parcels_with_ward_distances_permit_border500.csv \
	../output/boundary_distance_summary_permit_border500.csv \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.pdf \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_bins.csv \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_meta.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.pdf \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_bins.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_meta.csv \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.tex \
	../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.tex \
	../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.csv \
	../output/score_comparison_permit_border500_vs_baseline.csv \
	../output/score_comparison_permit_border500_vs_baseline_summary.csv \
	../output/score_comparison_permit_border500_vs_baseline_summary.tex \
	../output/score_comparison_permit_border500_vs_baseline_scatter.pdf \
	../output/density_score_robustness_comparison_permit_border500.csv \
	../output/density_score_robustness_comparison_permit_border500.tex \
	../output/parcel_sign_agreement_baseline_vs_permit_border500.csv

../output/building_permits_clean_border500_filtered.gpkg: filter_high_discretion_permits_near_boundary.R ../input/building_permits_clean_raw.gpkg ../input/ward_panel.gpkg | ../output
	Rscript filter_high_discretion_permits_near_boundary.R

../output/high_discretion_boundary_filter_summary.csv: ../output/building_permits_clean_border500_filtered.gpkg
	test -f "$@"

../output/high_discretion_boundary_distances.csv: ../output/building_permits_clean_border500_filtered.gpkg
	test -f "$@"

../output/permits_for_uncertainty_index_permit_border500.csv: ../../data_for_alderman_uncertainty_index/code/prepare_uncertainty_data.R ../output/building_permits_clean_border500_filtered.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/community_areas.geojson ../input/cta_stations.geojson ../input/city_boundary.geojson ../input/gis_osm_water_a_free_1.shp | ../output
	BUILDING_PERMITS_INPUT_PATH=../output/building_permits_clean_border500_filtered.gpkg OUTPUT_PERMITS_FOR_UNCERTAINTY_PATH=$@ Rscript ../../data_for_alderman_uncertainty_index/code/prepare_uncertainty_data.R

../output/alderman_uncertainty_index_$(BASELINE_SPEC).csv: ../../create_alderman_uncertainty_index/code/create_uncertainty_index.R ../../_lib/alderman_uncertainty_helpers.R ../output/permits_for_uncertainty_index_permit_border500.csv | ../output
	cd ../../create_alderman_uncertainty_index/code && PERMITS_INPUT_PATH=../../uncertainty_permit_border500_robustness/output/permits_for_uncertainty_index_permit_border500.csv UNCERTAINTY_OUTPUT_DIR=../../uncertainty_permit_border500_robustness/output Rscript create_uncertainty_index.R TRUE TRUE TRUE FALSE TRUE N_PERMITS LAG1 BOTH
	test -f ../output/uncertainty_index_$(BASELINE_SPEC).pdf
	test -f ../output/stage1_regression_$(BASELINE_SPEC).tex
	test -f ../output/stage2_regression_$(BASELINE_SPEC).tex

../output/uncertainty_index_$(BASELINE_SPEC).pdf: ../output/alderman_uncertainty_index_$(BASELINE_SPEC).csv
	test -f "$@"

../output/stage1_regression_$(BASELINE_SPEC).tex: ../output/alderman_uncertainty_index_$(BASELINE_SPEC).csv
	test -f "$@"

../output/stage2_regression_$(BASELINE_SPEC).tex: ../output/alderman_uncertainty_index_$(BASELINE_SPEC).csv
	test -f "$@"

../output/alderman_uncertainty_index_baseline.csv: ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../output
	ln -sf "$<" "$@"

../output/alderman_uncertainty_index_permit_border500.csv: ../output/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../output/uncertainty_index_$(BASELINE_SPEC).pdf ../output/stage1_regression_$(BASELINE_SPEC).tex ../output/stage2_regression_$(BASELINE_SPEC).tex | ../output
	ln -sf alderman_uncertainty_index_$(BASELINE_SPEC).csv ../output/alderman_uncertainty_index_permit_border500.csv
	ln -sf uncertainty_index_$(BASELINE_SPEC).pdf ../output/uncertainty_index_permit_border500.pdf
	ln -sf stage1_regression_$(BASELINE_SPEC).tex ../output/stage1_regression_permit_border500.tex
	ln -sf stage2_regression_$(BASELINE_SPEC).tex ../output/stage2_regression_permit_border500.tex

../output/uncertainty_index_permit_border500.pdf: ../output/alderman_uncertainty_index_permit_border500.csv
	test -f "$@"

../output/stage1_regression_permit_border500.tex: ../output/alderman_uncertainty_index_permit_border500.csv
	test -f "$@"

../output/stage2_regression_permit_border500.tex: ../output/alderman_uncertainty_index_permit_border500.csv
	test -f "$@"

../output/parcels_with_ward_distances_baseline.csv: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../output/alderman_uncertainty_index_baseline.csv | ../output
	PARCELS_INPUT_PATH=../input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances_baseline.csv MERGE_SUMMARY_OUTPUT_PATH=../output/boundary_distance_summary_baseline.csv Rscript ../../merge_in_scores/code/merge_scores.R ../output/alderman_uncertainty_index_baseline.csv uncertainty_index
	test -f ../output/boundary_distance_summary_baseline.csv

../output/boundary_distance_summary_baseline.csv: ../output/parcels_with_ward_distances_baseline.csv
	test -f "$@"

../output/parcels_with_ward_distances_permit_border500.csv: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../output/alderman_uncertainty_index_permit_border500.csv | ../output
	PARCELS_INPUT_PATH=../input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances_permit_border500.csv MERGE_SUMMARY_OUTPUT_PATH=../output/boundary_distance_summary_permit_border500.csv Rscript ../../merge_in_scores/code/merge_scores.R ../output/alderman_uncertainty_index_permit_border500.csv uncertainty_index
	test -f ../output/boundary_distance_summary_permit_border500.csv

../output/boundary_distance_summary_permit_border500.csv: ../output/parcels_with_ward_distances_permit_border500.csv
	test -f "$@"

../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_baseline.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_baseline.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf slope
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv

../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv: ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf
	test -f "$@"

../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv: ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf
	test -f "$@"

../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_baseline.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_baseline.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf slope
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv

../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv: ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf
	test -f "$@"

../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv: ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf
	test -f "$@"

../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.pdf: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_permit_border500.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_permit_border500.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.pdf slope
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_bins.csv
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_meta.csv

../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_bins.csv: ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.pdf
	test -f "$@"

../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_meta.csv: ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.pdf
	test -f "$@"

../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.pdf: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_permit_border500.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_permit_border500.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.pdf slope
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_bins.csv
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_meta.csv

../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_bins.csv: ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.pdf
	test -f "$@"

../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_meta.csv: ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.pdf
	test -f "$@"

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.tex: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_baseline.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_baseline.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
	test -f ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv

../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv: ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.tex
	test -f "$@"

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.tex: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_permit_border500.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_permit_border500.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.csv CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
	test -f ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.csv

../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.csv: ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.tex
	test -f "$@"

../output/score_comparison_permit_border500_vs_baseline.csv: compare_permit_border500_scores.R ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../output/alderman_uncertainty_index_permit_border500.csv | ../output
	Rscript compare_permit_border500_scores.R ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../output/alderman_uncertainty_index_permit_border500.csv ../output/score_comparison_permit_border500_vs_baseline.csv ../output/score_comparison_permit_border500_vs_baseline_summary.csv ../output/score_comparison_permit_border500_vs_baseline_summary.tex ../output/score_comparison_permit_border500_vs_baseline_scatter.pdf permit_border500 "Drop permits within 500 ft of ward borders"
	test -f ../output/score_comparison_permit_border500_vs_baseline_summary.csv
	test -f ../output/score_comparison_permit_border500_vs_baseline_summary.tex
	test -f ../output/score_comparison_permit_border500_vs_baseline_scatter.pdf

../output/score_comparison_permit_border500_vs_baseline_summary.csv: ../output/score_comparison_permit_border500_vs_baseline.csv
	test -f "$@"

../output/score_comparison_permit_border500_vs_baseline_summary.tex: ../output/score_comparison_permit_border500_vs_baseline.csv
	test -f "$@"

../output/score_comparison_permit_border500_vs_baseline_scatter.pdf: ../output/score_comparison_permit_border500_vs_baseline.csv
	test -f "$@"

../output/density_score_robustness_comparison_permit_border500.csv: summarize_permit_border500_density_robustness.R ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_meta.csv ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500_meta.csv ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_permit_border500.csv ../output/parcels_with_ward_distances_baseline.csv ../output/parcels_with_ward_distances_permit_border500.csv | ../output
	Rscript summarize_permit_border500_density_robustness.R ../output permit_border500 "Drop permits within 500 ft of ward borders" ../output/density_score_robustness_comparison_permit_border500.csv ../output/density_score_robustness_comparison_permit_border500.tex ../output/parcel_sign_agreement_baseline_vs_permit_border500.csv
	test -f ../output/density_score_robustness_comparison_permit_border500.tex
	test -f ../output/parcel_sign_agreement_baseline_vs_permit_border500.csv

../output/density_score_robustness_comparison_permit_border500.tex: ../output/density_score_robustness_comparison_permit_border500.csv
	test -f "$@"

../output/parcel_sign_agreement_baseline_vs_permit_border500.csv: ../output/density_score_robustness_comparison_permit_border500.csv
	test -f "$@"

../input/building_permits_clean_raw.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

../input/ward_controls.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	ln -sf "$<" "$@"

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	ln -sf "$<" "$@"

../input/cta_stations.geojson: ../../../data_raw/cta_stations.geojson | ../input
	ln -sf "$<" "$@"

../input/city_boundary.geojson: ../../../data_raw/Boundaries_-_City_20250920.geojson | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	ln -sf ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.* $(dir $@)

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	ln -sf "$<" "$@"

../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/building_permits_clean_raw.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/community_areas.geojson ../input/cta_stations.geojson ../input/city_boundary.geojson ../input/gis_osm_water_a_free_1.shp ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv

include ../../generic.make
