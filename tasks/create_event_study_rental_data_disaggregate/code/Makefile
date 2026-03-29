SHELL := bash
include ../../shell_functions.make

all: \
	../output/rental_listing_panel.parquet \
	../output/rental_listing_panel_2015.parquet \
	../output/rental_listing_panel_2023.parquet \
	../output/rental_listing_panel_support_by_event_time.csv \
	../output/rental_listing_panel_support_by_calendar_time.csv \
	../output/rental_listing_panel_repeat_listing_diagnostics.csv \
	../output/rental_listing_panel_assignment_stability.csv

../output/rental_listing_panel.parquet: create_listing_panel.R ../../_lib/canonical_geometry_helpers.R ../input/rent_with_ward_distances.parquet ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/block_treatment_panel.csv ../input/boundary_segments_1320ft.gpkg | ../output
	$(R) create_listing_panel.R

../output/rental_listing_panel_2015.parquet \
../output/rental_listing_panel_2023.parquet \
../output/rental_listing_panel_support_by_event_time.csv \
../output/rental_listing_panel_support_by_calendar_time.csv \
../output/rental_listing_panel_repeat_listing_diagnostics.csv \
../output/rental_listing_panel_assignment_stability.csv: ../output/rental_listing_panel.parquet
	test -f $@

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf $< $@

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

../input/census_blocks_2020.csv: ../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	ln -sf $< $@

../input/block_treatment_panel.csv: ../../merge_event_study_scores/output/block_treatment_panel.csv | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/rent_with_ward_distances.parquet ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/block_treatment_panel.csv ../input/boundary_segments_1320ft.gpkg
