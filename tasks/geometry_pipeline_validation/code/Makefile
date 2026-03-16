SHELL := bash
include ../../shell_functions.make

all: ../output/geometry_validation_report.md

../output/geometry_validation_report.md: validate_geometry_pipeline.R ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/parcels_pre_scores.csv ../input/parcel_segment_ids_coverage.csv ../input/parcels_with_ward_distances.csv ../input/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment.tex ../output/validation_density_far_meta.csv ../output/validation_density_dupac_meta.csv ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/segment_assignment_coverage_summary.csv ../input/fe_table_rental_bw500_pre_2023.csv ../input/fe_table_sales_bw500_year_quarter.csv ../input/before_fix/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment.tex ../input/before_fix/parcel_segment_ids_coverage.csv ../input/before_fix/segment_assignment_coverage_summary.csv | ../output
	$(R) validate_geometry_pipeline.R

../output/canonical_pair_universe.csv \
../output/geometry_coverage_summary.csv \
../output/geometry_sample_impact_summary.csv \
../output/geometry_pair_mismatch_report.csv \
../output/geometry_result_change_summary.csv \
../output/geometry_spotcheck_queue.csv: ../output/geometry_validation_report.md
	test -f $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf $< $@

../input/parcel_segment_ids_coverage.csv: ../../assign_segment_ids/output/parcel_segment_ids_coverage.csv | ../input
	ln -sf $< $@

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

../input/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment.tex: ../../border_pair_FE_regressions/output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment.tex | ../input
	ln -sf $< $@

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf $< $@

../output/validation_density_far_meta.csv: ../../spatial_rd_fe/code/spatial_rd_fe.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv | ../output
	RD_INPUT_PATH=../input/parcels_with_ward_distances.csv CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CLUSTER_LEVEL=segment $(R) ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 500 multifamily zonegroup_segment_year_additive ../output/validation_density_far.pdf slope
	test -f $@

../output/validation_density_dupac_meta.csv: ../../spatial_rd_fe/code/spatial_rd_fe.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv | ../output
	RD_INPUT_PATH=../input/parcels_with_ward_distances.csv CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CLUSTER_LEVEL=segment $(R) ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 500 multifamily zonegroup_segment_year_additive ../output/validation_density_dupac.pdf slope
	test -f $@

../input/sales_pre_scores.csv: ../../calculate_sale_distances/output/sales_pre_scores.csv | ../input
	ln -sf $< $@

../input/rent_pre_scores_full.parquet: ../../calculate_rent_distances/output/rent_pre_scores_full.parquet | ../input
	ln -sf $< $@

../input/segment_assignment_coverage_summary.csv: ../../assign_segment_ids_sales_rental/output/segment_assignment_coverage_summary.csv | ../input
	ln -sf $< $@

../input/fe_table_rental_bw500_pre_2023.csv: ../../rental_border_pair_fe/output/fe_table_rental_bw500_pre_2023.csv | ../input
	ln -sf $< $@

../input/fe_table_sales_bw500_year_quarter.csv: ../../sales_border_pair_fe/output/fe_table_sales_bw500_year_quarter.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/parcels_pre_scores.csv ../input/parcel_segment_ids_coverage.csv ../input/parcels_with_ward_distances.csv ../input/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_segment.tex ../input/confounded_pair_era_flags.csv ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/segment_assignment_coverage_summary.csv ../input/fe_table_rental_bw500_pre_2023.csv ../input/fe_table_sales_bw500_year_quarter.csv
