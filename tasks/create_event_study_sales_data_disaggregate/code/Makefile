SHELL := bash
include ../../shell_functions.make

all: \
	../output/sales_transaction_panel.parquet \
	../output/sales_transaction_panel_announcement.parquet \
	../output/sales_transaction_panel_2012.parquet \
	../output/sales_transaction_panel_2015.parquet \
	../output/sales_transaction_panel_2022.parquet \
	../output/sales_transaction_panel_2023.parquet \
	../output/sales_transaction_panel_support_by_event_time.csv \
	../output/sales_transaction_panel_support_by_calendar_time.csv \
	../output/sales_transaction_panel_assignment_stability.csv

../output/sales_transaction_panel.parquet: create_transaction_panel.R ../../_lib/canonical_geometry_helpers.R ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv ../input/boundary_segments_1320ft.gpkg | ../output
	$(R) $<

../output/sales_transaction_panel_announcement.parquet \
../output/sales_transaction_panel_2012.parquet \
../output/sales_transaction_panel_2015.parquet \
../output/sales_transaction_panel_2022.parquet \
../output/sales_transaction_panel_2023.parquet \
../output/sales_transaction_panel_support_by_event_time.csv \
../output/sales_transaction_panel_support_by_calendar_time.csv \
../output/sales_transaction_panel_assignment_stability.csv: ../output/sales_transaction_panel.parquet
	test -f $@

../input/sales_with_ward_distances.csv: ../../merge_event_study_scores/output/sales_with_ward_distances.csv | ../input
	ln -sf $< $@

../input/residential_improvements_panel.parquet: ../../process_residential_improvements_full/output/residential_improvements_panel.parquet | ../input
	ln -sf $< $@

../input/block_treatment_panel.csv: ../../merge_event_study_scores/output/block_treatment_panel.csv | ../input
	ln -sf $< $@

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv ../input/boundary_segments_1320ft.gpkg
