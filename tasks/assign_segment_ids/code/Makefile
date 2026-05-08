SHELL := bash
include ../../shell_functions.make

SEGMENT_LENGTH_M ?= 400
SEGMENT_BUFFER_M ?= 250
COVERAGE_BWS_M ?= 100,250

all: ../output/parcel_segment_ids.csv ../output/parcel_segment_ids_coverage.csv ../output/parcel_segment_ids_reason_summary.csv

../output/parcel_segment_ids.csv: assign_segment_ids.R ../../_lib/canonical_geometry_helpers.R ../input/parcels_pre_scores.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg | ../output
	# arg order: parcels_pre_scores parcels_with_geometry segment_gpkg output_lookup output_coverage output_reason segment_buffer_m coverage_bandwidths_m
	$(R) assign_segment_ids.R ../input/parcels_pre_scores.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg ../output/parcel_segment_ids.csv ../output/parcel_segment_ids_coverage.csv ../output/parcel_segment_ids_reason_summary.csv $(SEGMENT_BUFFER_M) "$(COVERAGE_BWS_M)"

../output/parcel_segment_ids_coverage.csv \
../output/parcel_segment_ids_reason_summary.csv: ../output/parcel_segment_ids.csv
	test -f ../output/parcel_segment_ids_coverage.csv

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_geometry.gpkg: ../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg: ../../border_segment_creation/output/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/parcels_pre_scores.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg
