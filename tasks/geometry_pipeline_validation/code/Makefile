SHELL := bash

all: ../output/geometry_validation_report.md

../output/geometry_validation_report.md: validate_geometry_pipeline.R ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/parcels_pre_scores.csv ../input/parcel_segment_ids_coverage.csv ../input/parcels_with_ward_distances.csv ../input/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment.tex ../input/nonparametric_rd_density_linear_display_4panel_500ft_all_multifamily_bins5_estimates.csv ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/segment_assignment_coverage_summary.csv ../input/fe_table_rental_bw500_pre_2023.csv ../input/fe_table_sales_bw500_year_quarter.csv | ../output
	$(R) validate_geometry_pipeline.R

../output/canonical_pair_universe.csv \
../output/geometry_coverage_summary.csv \
../output/geometry_sample_impact_summary.csv \
../output/geometry_pair_mismatch_report.csv \
../output/geometry_result_change_summary.csv \
../output/geometry_spotcheck_queue.csv: ../output/geometry_validation_report.md
	@test -s "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids_coverage.csv: ../../assign_segment_ids/output/parcel_segment_ids_coverage.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment.tex: ../../border_pair_FE_regressions/output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment.tex | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/nonparametric_rd_density_linear_display_4panel_500ft_all_multifamily_bins5_estimates.csv: ../../nonparametric_rd_density_linear_display/output/nonparametric_rd_density_linear_display_4panel_500ft_all_multifamily_bins5_estimates.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_pre_scores.csv: ../../calculate_sale_distances/output/sales_pre_scores.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/rent_pre_scores_full.parquet: ../../calculate_rent_distances/output/rent_pre_scores_full.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/segment_assignment_coverage_summary.csv: ../../assign_segment_ids_sales_rental/output/segment_assignment_coverage_summary.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/fe_table_rental_bw500_pre_2023.csv: ../../rental_border_pair_fe/output/fe_table_rental_bw500_pre_2023.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/fe_table_sales_bw500_year_quarter.csv: ../../sales_border_pair_fe/output/fe_table_sales_bw500_year_quarter.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"


link-inputs: ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/parcels_pre_scores.csv ../input/parcel_segment_ids_coverage.csv ../input/parcels_with_ward_distances.csv ../input/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment.tex ../input/nonparametric_rd_density_linear_display_4panel_500ft_all_multifamily_bins5_estimates.csv ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/segment_assignment_coverage_summary.csv ../input/fe_table_rental_bw500_pre_2023.csv ../input/fe_table_sales_bw500_year_quarter.csv

include ../../shell_functions.make
include ../../generic.make
