SHELL := bash

BANDWIDTH_FT ?= 500
SAMPLE ?= all

all: ../output/rd_sample_flag_balance.csv

../output/rd_sample_flag_balance.csv: run_rental_rd_quality_audit.R ../../../_lib/canonical_geometry_helpers.R ../input/rent_with_ward_distances.parquet ../input/boundary_segments_1320ft.gpkg | ../output
	$(R) $< $(BANDWIDTH_FT) $(SAMPLE)

../output/rd_segment_contract_audit.csv \
../output/rd_segment_month_flag_imbalance.csv \
../output/rd_sample_definition_summary.csv: ../output/rd_sample_flag_balance.csv
	@test -s "$@"

../input/rent_with_ward_distances.parquet: ../../../merge_rent_scores/output/rent_with_ward_distances_full_through2022.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet ../input/boundary_segments_1320ft.gpkg

include ../../../shell_functions.make
include ../../../generic.make
