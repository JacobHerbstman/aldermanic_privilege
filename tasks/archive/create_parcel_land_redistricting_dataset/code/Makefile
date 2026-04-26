SHELL := bash
include ../../shell_functions.make

all: ../output/parcel_land_redistricting_panel.parquet ../output/parcel_land_redistricting_diagnostics.csv ../output/parcel_land_redistricting_support.csv

../output/parcel_land_redistricting_panel.parquet: build_parcel_land_redistricting_dataset.R ../../_lib/canonical_geometry_helpers.R ../input/parcel_land_value_panel.parquet ../input/census_blocks_2010.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/block_treatment_panel.csv | ../output
	$(R) build_parcel_land_redistricting_dataset.R ../input/parcel_land_value_panel.parquet ../input/census_blocks_2010.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/block_treatment_panel.csv ../output/parcel_land_redistricting_panel.parquet ../output/parcel_land_redistricting_diagnostics.csv ../output/parcel_land_redistricting_support.csv

../output/parcel_land_redistricting_diagnostics.csv \
../output/parcel_land_redistricting_support.csv: ../output/parcel_land_redistricting_panel.parquet
	test -f $@

../input/parcel_land_value_panel.parquet: ../../parcel_land_value_panel/output/parcel_land_value_panel.parquet | ../input
	ln -sf $< $@

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

../input/block_treatment_panel.csv: ../../merge_event_study_scores/output/block_treatment_panel.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcel_land_value_panel.parquet ../input/census_blocks_2010.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/block_treatment_panel.csv
