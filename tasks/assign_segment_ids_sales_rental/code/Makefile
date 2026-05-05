SHELL := bash
include ../../shell_functions.make

all: ../output/sales_pre_scores_with_segments.csv ../output/rent_pre_scores_full_with_segments.parquet ../output/segment_assignment_coverage_summary.csv ../output/segment_assignment_spotcheck_queue.csv ../output/segment_assignment_reason_summary.csv

../output/sales_pre_scores_with_segments.csv: assign_segment_ids_sales_rental.R ../../_lib/canonical_geometry_helpers.R ../input/sales_pre_scores.csv ../input/boundary_segments_1320ft.gpkg | ../output
	# arg order: mode sales_input segment_gpkg out_sales
	$(R) assign_segment_ids_sales_rental.R \
		sales \
		../input/sales_pre_scores.csv \
		../input/boundary_segments_1320ft.gpkg \
		../output/sales_pre_scores_with_segments.csv

../output/rent_pre_scores_full_with_segments.parquet \
../output/segment_assignment_coverage_summary.csv \
../output/segment_assignment_spotcheck_queue.csv \
../output/segment_assignment_reason_summary.csv: assign_segment_ids_sales_rental.R ../../_lib/canonical_geometry_helpers.R ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/boundary_segments_1320ft.gpkg | ../output
	# arg order: mode sales_input rent_input segment_gpkg out_sales out_rent out_coverage out_spotcheck out_reason
	$(R) assign_segment_ids_sales_rental.R \
		all \
		../input/sales_pre_scores.csv \
		../input/rent_pre_scores_full.parquet \
		../input/boundary_segments_1320ft.gpkg \
		../output/sales_pre_scores_with_segments.csv \
		../output/rent_pre_scores_full_with_segments.parquet \
		../output/segment_assignment_coverage_summary.csv \
		../output/segment_assignment_spotcheck_queue.csv \
		../output/segment_assignment_reason_summary.csv
	test -f ../output/sales_pre_scores_with_segments.csv
	test -f ../output/rent_pre_scores_full_with_segments.parquet
	test -f ../output/segment_assignment_coverage_summary.csv
	test -f ../output/segment_assignment_spotcheck_queue.csv
	test -f ../output/segment_assignment_reason_summary.csv

../input/sales_pre_scores.csv: ../../calculate_sale_distances/output/sales_pre_scores.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/rent_pre_scores_full.parquet: ../../calculate_rent_distances/output/rent_pre_scores_full.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/boundary_segments_1320ft.gpkg
