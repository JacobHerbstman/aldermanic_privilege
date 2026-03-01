SHELL := bash
include ../../shell_functions.make

all: ../output/sales_pre_scores_with_segments.csv ../output/rent_pre_scores_full_with_segments.parquet ../output/segment_assignment_coverage_summary.csv ../output/segment_assignment_spotcheck_queue.csv

../output/sales_pre_scores_with_segments.csv ../output/rent_pre_scores_full_with_segments.parquet ../output/segment_assignment_coverage_summary.csv ../output/segment_assignment_spotcheck_queue.csv: assign_segment_ids_sales_rental.R ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/boundary_segments_1320ft.gpkg | ../output
	# arg order: sales_input rent_input segment_gpkg out_sales out_rent out_coverage out_spotcheck
	$(R) assign_segment_ids_sales_rental.R \
		../input/sales_pre_scores.csv \
		../input/rent_pre_scores_full.parquet \
		../input/boundary_segments_1320ft.gpkg \
		../output/sales_pre_scores_with_segments.csv \
		../output/rent_pre_scores_full_with_segments.parquet \
		../output/segment_assignment_coverage_summary.csv \
		../output/segment_assignment_spotcheck_queue.csv

../input/sales_pre_scores.csv: ../../calculate_sale_distances/output/sales_pre_scores.csv | ../input
	ln -sf $< $@

../input/rent_pre_scores_full.parquet: ../../calculate_rent_distances/output/rent_pre_scores_full.parquet | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/boundary_segments_1320ft.gpkg
