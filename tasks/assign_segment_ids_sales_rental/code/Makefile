SHELL := bash
include ../../shell_functions.make

SEGMENT_LENGTH_M ?= 400
SEGMENT_BUFFER_M ?= 250
COVERAGE_BWS_M ?= 100,250
SPOTCHECK_BW_M ?= 250

all: ../output/sales_pre_scores_with_segments.csv ../output/rent_pre_scores_full_with_segments.parquet ../output/segment_assignment_coverage_summary.csv ../output/segment_assignment_spotcheck_queue.csv ../output/segment_assignment_reason_summary.csv

../output/sales_pre_scores_with_segments.csv: assign_segment_ids_sales_rental.R ../../_lib/canonical_geometry_helpers.R ../input/sales_pre_scores.csv ../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg | ../output
	# arg order: mode sales_input segment_gpkg out_sales segment_buffer_m coverage_bandwidths_m
	$(R) assign_segment_ids_sales_rental.R \
		sales \
		../input/sales_pre_scores.csv \
		../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg \
		../output/sales_pre_scores_with_segments.csv \
		$(SEGMENT_BUFFER_M) \
		"$(COVERAGE_BWS_M)"

../output/rent_pre_scores_full_with_segments.parquet \
../output/segment_assignment_coverage_summary.csv \
../output/segment_assignment_spotcheck_queue.csv \
../output/segment_assignment_reason_summary.csv: assign_segment_ids_sales_rental.R ../../_lib/canonical_geometry_helpers.R ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg | ../output
	# arg order: mode sales_input rent_input segment_gpkg out_sales out_rent out_coverage out_spotcheck out_reason segment_buffer_m coverage_bandwidths_m spotcheck_bandwidth_m
	$(R) assign_segment_ids_sales_rental.R \
		all \
		../input/sales_pre_scores.csv \
		../input/rent_pre_scores_full.parquet \
		../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg \
		../output/sales_pre_scores_with_segments.csv \
		../output/rent_pre_scores_full_with_segments.parquet \
		../output/segment_assignment_coverage_summary.csv \
		../output/segment_assignment_spotcheck_queue.csv \
		../output/segment_assignment_reason_summary.csv \
		$(SEGMENT_BUFFER_M) \
		"$(COVERAGE_BWS_M)" \
		$(SPOTCHECK_BW_M)
	test -f ../output/sales_pre_scores_with_segments.csv
	test -f ../output/rent_pre_scores_full_with_segments.parquet
	test -f ../output/segment_assignment_coverage_summary.csv
	test -f ../output/segment_assignment_spotcheck_queue.csv
	test -f ../output/segment_assignment_reason_summary.csv

../input/sales_pre_scores.csv: ../../calculate_sale_distances/output/sales_pre_scores.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/rent_pre_scores_full.parquet: ../../calculate_rent_distances/output/rent_pre_scores_full.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg: ../../border_segment_creation/output/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg
