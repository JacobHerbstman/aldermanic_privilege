SHELL := bash
include ../../shell_functions.make

all: ../output/rental_listing_panel.parquet

../output/rental_listing_panel.parquet: create_listing_panel.R ../input/rent_with_ward_distances.parquet ../input/census_blocks_2010.csv ../input/block_treatment_panel.csv ../input/boundary_segments_1320ft.gpkg | ../output
	$(R) create_listing_panel.R

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf $< $@

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

../input/block_treatment_panel.csv: ../../merge_event_study_scores/output/block_treatment_panel.csv | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/rent_with_ward_distances.parquet ../input/census_blocks_2010.csv ../input/block_treatment_panel.csv ../input/boundary_segments_1320ft.gpkg
