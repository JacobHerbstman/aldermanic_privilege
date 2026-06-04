all: ../output/geometry_validation_report.md

../output/geometry_validation_report.md: validate_geometry_pipeline.R ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/parcels_with_ward_distances.csv ../input/fe_table_500ft_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet | ../output
	Rscript $<

../output/canonical_pair_universe.csv \
../output/geometry_coverage_summary.csv \
../output/geometry_sample_impact_summary.csv \
../output/geometry_pair_mismatch_report.csv \
../output/geometry_result_change_summary.csv \
../output/geometry_spotcheck_queue.csv: ../output/geometry_validation_report.md
	@test -s "$@"

../input/ward_pair_boundaries.gpkg: ../../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_pre_scores.csv: ../../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/fe_table_500ft_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex: ../../../border_pair_FE_regressions/output/fe_table_500ft_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_pre_scores.csv: ../../../calculate_sale_distances/output/sales_pre_scores.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/rent_pre_scores_full.parquet: ../../../calculate_rent_distances/output/rent_pre_scores_full.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/parcels_with_ward_distances.csv ../input/fe_table_500ft_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet

include ../../../generic.make
