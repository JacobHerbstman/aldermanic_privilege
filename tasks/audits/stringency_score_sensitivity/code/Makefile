all: ../output/alderman_scores.csv ../output/density_score_sensitivity_models.csv ../output/density_score_sensitivity_summary.csv ../output/density_drop_income_rd_4panel.png ../output/density_rd_visualization_4methods.png ../output/drop_income_multifamily_continuous_bandwidth_path.pdf ../output/drop_income_density_short_paper_robustness.csv ../output/permit_volume_correlation_definition_audit.csv ../output/selected_low_discretion_density_placebo.tex ../output/permit_drop_income_event_2panel.png ../output/sales_drop_income_event_2panel.png ../output/sales_drop_income_event_2panel_1000ft.png ../output/rezoning_drop_income_event_3panel.png ../output/drop_income_alderman_scores.csv ../output/drop_income_workload_score_comparison.csv ../output/drop_income_rent_sales_rd.png ../output/drop_income_stage1_regression.tex ../output/drop_income_rent_placebo_rd_main_style.pdf ../output/drop_income_sales_placebo_rd_main_style.pdf

../output/alderman_scores.csv: build_score_grid.R ../../../_lib/alderman_uncertainty_helpers.R ../input/permit_nonnegative_score_sample.csv | ../output
	Rscript $<

../output/score_specifications.csv \
../output/score_metadata.csv \
../output/score_stage1_terms.csv: ../output/alderman_scores.csv
	@test -s "$@"

../output/density_score_sensitivity_models.csv: run_density_score_grid.R ../input/density_score_sensitivity_sample.csv ../input/alderman_scores.csv ../input/score_specifications.csv | ../output
	Rscript $<

../output/density_score_side_assignments.csv: ../output/density_score_sensitivity_models.csv
	@test -s "$@"

../output/density_score_sensitivity_summary.csv: summarize_score_grid.R ../input/alderman_scores.csv ../input/score_specifications.csv ../input/density_score_sensitivity_models.csv ../input/density_score_side_assignments.csv | ../output
	Rscript $<

../output/score_comparisons.csv \
../output/score_rank_changes.csv \
../output/density_score_sensitivity_comparisons.csv \
../output/density_score_sensitivity_all.png \
../output/density_score_sensitivity_multifamily.png \
../output/score_sensitivity_correlations.png: ../output/density_score_sensitivity_summary.csv
	@test -s "$@"

../output/density_drop_income_rd_4panel.png: plot_density_drop_income.R ../../../_lib/border_pair_helpers.R ../input/alderman_scores.csv ../input/density_score_sensitivity_sample.csv ../input/density_score_sensitivity_models.csv | ../output
	Rscript $<

../output/density_drop_income_rd_4panel.pdf \
../output/density_drop_income_rd_partial_4panel.pdf \
../output/density_drop_income_rd_partial_4panel.png \
../output/density_drop_income_rd_original_style_4panel.pdf \
../output/density_drop_income_rd_original_style_4panel.png \
../output/density_drop_income_rd_original_style_estimates.tex \
../output/drop_income_density_main_table.tex \
../output/density_drop_income_rd_all_far.png \
../output/density_drop_income_rd_multifamily_far.png \
../output/density_drop_income_rd_all_dupac.png \
../output/density_drop_income_rd_multifamily_dupac.png: ../output/density_drop_income_rd_4panel.png
	@test -s "$@"

../output/density_drop_income_rd_bin_coefficients_4panel.png: plot_density_drop_income_bin_coefficients.R ../../../_lib/border_pair_helpers.R ../input/alderman_scores.csv ../input/density_score_sensitivity_sample.csv ../input/density_score_sensitivity_models.csv | ../output
	Rscript $<

../output/density_drop_income_rd_bin_coefficients_4panel.pdf: ../output/density_drop_income_rd_bin_coefficients_4panel.png
	@test -s "$@"

../output/density_rd_visualization_4methods.png: compare_density_rd_visualizations.R ../../../_lib/border_pair_helpers.R ../input/alderman_scores.csv ../input/density_score_sensitivity_sample.csv ../input/density_score_sensitivity_models.csv | ../output
	Rscript $<

../output/density_rd_visualization_4methods.pdf \
../output/density_rd_visualization_4methods_results.csv \
../output/density_rd_method_1_raw.png \
../output/density_rd_method_1_raw.pdf \
../output/density_rd_method_2_adjusted.png \
../output/density_rd_method_2_adjusted.pdf \
../output/density_rd_method_3_residualized.png \
../output/density_rd_method_3_residualized.pdf \
../output/density_rd_method_4_hdfe_bins.png \
../output/density_rd_method_4_hdfe_bins.pdf: ../output/density_rd_visualization_4methods.png
	@test -s "$@"

../output/drop_income_multifamily_continuous_bandwidth_path.pdf: plot_density_continuous_bandwidth_path.R ../../../_lib/border_pair_helpers.R ../input/alderman_scores.csv ../input/density_placebo_1000ft_sample.csv ../input/density_score_sensitivity_models.csv | ../output
	Rscript $<

../output/drop_income_multifamily_continuous_bandwidth_path.png \
../output/drop_income_multifamily_continuous_bandwidth_path.csv \
../output/drop_income_all_construction_continuous_bandwidth_path.pdf \
../output/drop_income_all_construction_continuous_bandwidth_path.png \
../output/drop_income_all_construction_continuous_bandwidth_path.csv: ../output/drop_income_multifamily_continuous_bandwidth_path.pdf
	@test -s "$@"

../output/permit_volume_correlation_definition_audit.csv: audit_permit_volume_correlation.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv | ../output
	Rscript $<

../output/selected_permit_processing_summary.tex \
../output/selected_permit_volume_correlation.tex: ../output/permit_volume_correlation_definition_audit.csv
	@test -s "$@"

../output/selected_low_discretion_density_placebo.tex: build_selected_low_discretion_density_table.R ../../../_lib/border_pair_helpers.R ../input/density_score_sensitivity_sample.csv ../input/low_discretion_alderman_scores.csv | ../output
	Rscript $<

../output/selected_low_discretion_density_placebo.csv: ../output/selected_low_discretion_density_placebo.tex
	@test -s "$@"

../output/density_placebo_1000ft_sample.csv: build_density_placebo_1000ft_sample.R ../../../_lib/canonical_geometry_helpers.R ../input/density_project_lineage.csv ../input/density_parcel_address_lineage_evidence.csv ../input/density_parcel_address_recovered_model_rows.csv ../input/density_historical_recovered_exact_rows.csv ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_1320ft.gpkg ../input/zoning_data_clean.gpkg | ../output
	Rscript $<

../output/density_placebo_1000ft_sample_summary.csv: ../output/density_placebo_1000ft_sample.csv
	@test -s "$@"

../output/drop_income_density_short_paper_robustness.csv: run_density_short_paper_robustness.R ../../../_lib/border_pair_helpers.R ../input/alderman_scores.csv ../input/density_score_sensitivity_sample.csv ../input/density_project_lineage.csv ../input/density_parcel_address_lineage_evidence.csv ../input/density_parcel_address_recovered_model_rows.csv ../input/density_historical_recovered_exact_rows.csv ../input/parcels_with_ward_distances.csv ../input/density_placebo_1000ft_sample.csv | ../output
	Rscript $<

../output/drop_income_density_short_paper_robustness.pdf \
../output/drop_income_density_short_paper_robustness.png \
../output/preview_density_placebo_neg500ft_all_far.pdf \
../output/preview_density_placebo_neg500ft_multifamily_far.pdf \
../output/preview_density_placebo_neg500ft_all_dupac.pdf \
../output/preview_density_placebo_neg500ft_multifamily_dupac.pdf \
../output/preview_density_placebo_pos500ft_all_far.pdf \
../output/preview_density_placebo_pos500ft_multifamily_far.pdf \
../output/preview_density_placebo_pos500ft_all_dupac.pdf \
../output/preview_density_placebo_pos500ft_multifamily_dupac.pdf \
../output/preview_density_placebo_neg1000ft_all_far.pdf \
../output/preview_density_placebo_neg1000ft_multifamily_far.pdf \
../output/preview_density_placebo_neg1000ft_all_dupac.pdf \
../output/preview_density_placebo_neg1000ft_multifamily_dupac.pdf \
../output/preview_density_placebo_pos1000ft_all_far.pdf \
../output/preview_density_placebo_pos1000ft_multifamily_far.pdf \
../output/preview_density_placebo_pos1000ft_all_dupac.pdf \
../output/preview_density_placebo_pos1000ft_multifamily_dupac.pdf \
../output/preview_density_donut25ft_all_far.pdf \
../output/preview_density_donut25ft_multifamily_far.pdf \
../output/preview_density_donut25ft_all_dupac.pdf \
../output/preview_density_donut25ft_multifamily_dupac.pdf \
../output/preview_density_donut50ft_all_far.pdf \
../output/preview_density_donut50ft_multifamily_far.pdf \
../output/preview_density_donut50ft_all_dupac.pdf \
../output/preview_density_donut50ft_multifamily_dupac.pdf: ../output/drop_income_density_short_paper_robustness.csv
	@test -s "$@"

../output/permit_drop_income_event_2panel.png: plot_permit_event_drop_income.R ../input/alderman_scores.csv ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $<

../output/permit_drop_income_event_high.png \
../output/permit_drop_income_event_low.png \
../output/permit_drop_income_event_high_stable.png \
../output/permit_drop_income_event_estimates.csv \
../output/permit_drop_income_event_summary.csv \
../output/permit_drop_income_did_table.tex: ../output/permit_drop_income_event_2panel.png
	@test -s "$@"

../output/sales_drop_income_event_2panel.png: run_sales_event_drop_income.R ../input/alderman_scores.csv ../input/sales_transaction_panel_2015.parquet ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< 500

../output/sales_drop_income_event_hedonics_amenities.png \
../output/sales_drop_income_event_plus_pre_price.png \
../output/sales_drop_income_event_estimates.csv \
../output/sales_drop_income_event_summary.csv \
../output/sales_drop_income_score_comparison.csv: ../output/sales_drop_income_event_2panel.png
	@test -s "$@"

../output/sales_drop_income_event_2panel_1000ft.png: run_sales_event_drop_income.R ../input/alderman_scores.csv ../input/sales_transaction_panel_2015.parquet ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< 1000

../output/sales_drop_income_event_hedonics_amenities_1000ft.png \
../output/sales_drop_income_event_plus_pre_price_1000ft.png \
../output/sales_drop_income_event_estimates_1000ft.csv \
../output/sales_drop_income_event_summary_1000ft.csv \
../output/sales_drop_income_score_comparison_1000ft.csv: ../output/sales_drop_income_event_2panel_1000ft.png
	@test -s "$@"

../output/rezoning_drop_income_event_3panel.png: run_rezoning_event_drop_income.R ../input/alderman_scores.csv ../input/permit_block_year_panel_2015.parquet ../input/rezoning_census_blocks_20101101_20201231.csv ../input/rezoning_matter_block_bridge_20101101_20201231.csv | ../output
	Rscript $<

../output/rezoning_drop_income_event_any_rezoning.png \
../output/rezoning_drop_income_event_any_upzone.png \
../output/rezoning_drop_income_event_far_change_total.png \
../output/rezoning_drop_income_event_estimates.csv \
../output/rezoning_drop_income_event_summary.csv \
../output/rezoning_drop_income_score_comparison.csv: ../output/rezoning_drop_income_event_3panel.png
	@test -s "$@"

../output/drop_income_alderman_scores.csv: export_drop_income_results.R ../output/alderman_scores.csv ../output/score_stage1_terms.csv ../output/score_metadata.csv | ../output
	Rscript $<

../output/drop_income_first_stage_results.csv: ../output/drop_income_alderman_scores.csv
	@test -s "$@"

../output/drop_income_workload_score_comparison.csv: compare_drop_income_workload_scores.R ../../../_lib/alderman_uncertainty_helpers.R ../input/permit_nonnegative_score_sample.csv ../output/alderman_scores.csv | ../output
	Rscript $<

../output/drop_income_workload_score_summary.csv: ../output/drop_income_workload_score_comparison.csv
	@test -s "$@"

../output/drop_income_rent_sales_rd.png: run_rent_sales_drop_income.R ../output/alderman_scores.csv ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet | ../output
	Rscript $<

../output/drop_income_rent_rd.png \
../output/drop_income_sales_rd.png \
../output/drop_income_rent_sales_results.csv \
../output/drop_income_rent_sales_control_robustness.csv \
../output/drop_income_rent_sales_reilly_models.csv \
../output/drop_income_rent_sales_flipped_pairs.csv \
../output/drop_income_sales_pair_leave_out.csv: ../output/drop_income_rent_sales_rd.png
	@test -s "$@"

../output/drop_income_stage1_regression.tex: build_short_paper_score_exhibits.R ../input/alderman_scores.csv ../input/score_stage1_terms.csv ../input/score_metadata.csv ../input/chicago_alderman_panel.csv ../input/ward_panel.gpkg ../input/residualized_low_vs_high_processing_alderman_wide.csv | ../output
	Rscript $<

../output/drop_income_score_distribution.pdf \
../output/drop_income_score_distribution.png \
../output/drop_income_score_map.pdf \
../output/drop_income_score_map.png \
../output/drop_income_predecessor_successor.pdf \
../output/drop_income_predecessor_successor.png \
../output/drop_income_predecessor_successor_summary.csv \
../output/drop_income_low_discretion_placebo.csv \
../output/drop_income_low_discretion_placebo.tex \
../output/drop_income_short_paper_scores.csv: ../output/drop_income_stage1_regression.tex
	@test -s "$@"

../output/rent_with_ward_distances_full.parquet: build_price_drop_income_inputs.R ../input/alderman_scores.csv ../input/rent_with_ward_distances_full_source.parquet ../input/sales_with_hedonics_amenities_source.parquet | ../output
	Rscript $<

../output/sales_with_hedonics_amenities.parquet: ../output/rent_with_ward_distances_full.parquet
	@test -s "$@"

../output/drop_income_rent_placebo_rd_main_style.pdf: build_price_rd_main_style_plots_drop_income.R ../../../_lib/amenity_distance_helpers.R ../input/rent_with_ward_distances_full_drop_income.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript $< rent 500 10 1000

../output/drop_income_sales_placebo_rd_main_style.pdf: build_price_rd_main_style_plots_drop_income.R ../input/sales_with_hedonics_amenities_drop_income.parquet | ../output
	Rscript $< sales 500 10 1000

../output/drop_income_rent_placebo_rd_results.csv: ../output/drop_income_rent_placebo_rd_main_style.pdf
	@test -s "$@"

../output/rent_placebo_rd_no_controls.pdf: build_price_rd_main_style_plots_drop_income.R ../../../_lib/amenity_distance_helpers.R ../input/rent_with_ward_distances_full_drop_income.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript $< rent 500 10 1000 no_controls_fixed_sample

../output/rent_placebo_rd_no_controls.png \
../output/rent_placebo_rd_no_controls_results.csv: ../output/rent_placebo_rd_no_controls.pdf
	@test -s "$@"

../output/sales_placebo_rd_no_controls.pdf: build_price_rd_main_style_plots_drop_income.R ../input/sales_with_hedonics_amenities_drop_income.parquet | ../output
	Rscript $< sales 500 10 1000 no_controls_fixed_sample

../output/sales_placebo_rd_no_controls.png \
../output/sales_placebo_rd_no_controls_results.csv: ../output/sales_placebo_rd_no_controls.pdf
	@test -s "$@"

../output/drop_income_sales_placebo_rd_results.csv: ../output/drop_income_sales_placebo_rd_main_style.pdf
	@test -s "$@"

../input/permit_nonnegative_score_sample.csv: ../../paper_code_alignment_audit/output/permit_nonnegative_score_sample.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_score_sensitivity_sample.csv: ../../density_project_lineage/output/density_score_sensitivity_sample.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_scores.csv: ../output/alderman_scores.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/score_specifications.csv: ../output/score_specifications.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_score_sensitivity_models.csv: ../output/density_score_sensitivity_models.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/score_stage1_terms.csv: ../output/score_stage1_terms.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/score_metadata.csv: ../output/score_metadata.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_score_side_assignments.csv: ../output/density_score_side_assignments.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rental_rd_characteristics_panel_bw500.parquet: ../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rezoning_census_blocks_20101101_20201231.csv: ../../../rezoning_census_block_assignment/output/rezoning_census_blocks_20101101_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rezoning_matter_block_bridge_20101101_20201231.csv: ../../../rezoning_census_block_assignment/output/rezoning_matter_block_bridge_20101101_20201231.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_project_lineage.csv: ../../density_project_lineage/output/density_project_lineage.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_parcel_address_lineage_evidence.csv: ../../density_project_lineage/output/density_parcel_address_lineage_evidence.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_parcel_address_recovered_model_rows.csv: ../../density_project_lineage/output/density_parcel_address_recovered_model_rows.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_historical_recovered_exact_rows.csv: ../../density_project_lineage/input/density_historical_recovered_exact_rows.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../density_project_lineage/input/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_placebo_1000ft_sample.csv: ../output/density_placebo_1000ft_sample.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_geometry.gpkg: ../../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/zoning_data_clean.gpkg: ../../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/low_discretion_alderman_scores.csv: ../../../new_construction_score_variants/output/alderman_uncertainty_index_low_discretion_residualized.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/residualized_low_vs_high_processing_alderman_wide.csv: ../../../within_ward_strictness/output/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rent_with_ward_distances_full_source.parquet: ../../../merge_rent_scores/output/rent_with_ward_distances_full_through2022.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities_source.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rent_with_ward_distances_full_drop_income.parquet: ../output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities_drop_income.parquet: ../output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../../prepare_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../../prepare_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cta_stops.gpkg: ../../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.%: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_nonnegative_score_sample.csv ../input/density_score_sensitivity_sample.csv ../input/alderman_scores.csv ../input/score_specifications.csv ../input/score_stage1_terms.csv ../input/score_metadata.csv ../input/density_score_sensitivity_models.csv ../input/density_score_side_assignments.csv ../input/permit_block_year_panel_2015.parquet ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/sales_transaction_panel_2015.parquet ../input/rezoning_census_blocks_20101101_20201231.csv ../input/rezoning_matter_block_bridge_20101101_20201231.csv ../input/density_project_lineage.csv ../input/density_parcel_address_lineage_evidence.csv ../input/density_parcel_address_recovered_model_rows.csv ../input/density_historical_recovered_exact_rows.csv ../input/parcels_with_ward_distances.csv ../input/density_placebo_1000ft_sample.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_1320ft.gpkg ../input/zoning_data_clean.gpkg ../input/chicago_alderman_panel.csv ../input/ward_panel.gpkg ../input/building_permits_clean.gpkg ../input/low_discretion_alderman_scores.csv ../input/residualized_low_vs_high_processing_alderman_wide.csv ../input/rent_with_ward_distances_full_source.parquet ../input/sales_with_hedonics_amenities_source.parquet ../input/rent_with_ward_distances_full_drop_income.parquet ../input/sales_with_hedonics_amenities_drop_income.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../../generic.make
