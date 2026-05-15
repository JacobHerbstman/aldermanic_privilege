SHELL := bash

SEGMENT_LENGTH_M ?= 400
SEGMENT_BUFFER_M ?= 250
PANEL_MAX_DISTANCE_M ?= 800

all: \
	../output/rental_listing_panel.parquet \
	../output/rental_listing_panel_2015.parquet \
	../output/rental_listing_panel_2023.parquet \
	../output/rental_listing_panel_support_by_event_time.csv \
	../output/rental_listing_panel_support_by_calendar_time.csv \
	../output/rental_listing_panel_repeat_listing_diagnostics.csv \
	../output/rental_listing_panel_assignment_stability.csv

../output/rental_listing_panel.parquet: create_listing_panel.R ../../_lib/canonical_geometry_helpers.R ../input/rent_with_ward_distances.parquet ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/block_treatment_panel.csv ../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg | ../output
	$(R) create_listing_panel.R $(SEGMENT_BUFFER_M) $(PANEL_MAX_DISTANCE_M)

../output/rental_listing_panel_2015.parquet \
../output/rental_listing_panel_2023.parquet \
../output/rental_listing_panel_support_by_event_time.csv \
../output/rental_listing_panel_support_by_calendar_time.csv \
../output/rental_listing_panel_repeat_listing_diagnostics.csv \
../output/rental_listing_panel_assignment_stability.csv: ../output/rental_listing_panel.parquet
	@test -s "$@"

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2020.csv: ../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_treatment_panel.csv: ../../merge_event_study_scores/output/block_treatment_panel.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg: ../../border_segment_creation/output/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"


link-inputs: ../input/rent_with_ward_distances.parquet ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/block_treatment_panel.csv ../input/boundary_segments_$(SEGMENT_LENGTH_M)m.gpkg

include ../../shell_functions.make
include ../../generic.make
