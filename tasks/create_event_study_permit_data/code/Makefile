SHELL := bash
include ../../shell_functions.make

all: \
	../output/permit_block_year_panel.parquet \
	../output/permit_block_year_panel_2015.parquet \
	../output/permit_block_year_panel_2023.parquet \
	../output/permit_block_year_panel_support_by_event_time.csv \
	../output/permit_block_year_panel_support_by_calendar_time.csv \
	../output/permit_block_year_panel_assignment_stability.csv \
	../output/permit_block_year_panel_zero_share_summary.csv \
	../output/permit_block_year_panel_outcome_totals.csv \
	../output/permit_block_assignment_missing_2010.csv \
	../output/permit_block_assignment_missing_2010_summary.csv \
	../output/permit_block_assignment_missing_2020.csv \
	../output/permit_block_assignment_missing_2020_summary.csv \
	../output/permit_block_assignment_review_log.csv \
	../output/permit_block_assignment_review_summary.csv \
	../output/permit_unit_increase_audit.csv \
	../output/permit_unit_increase_audit_summary.csv

../output/permit_block_year_panel.parquet: create_permit_block_year_panel.R manual_permit_block_review.csv ../../_lib/canonical_geometry_helpers.R ../input/building_permits_clean.gpkg ../input/building_permits_text_features.csv.gz ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/ward_panel.gpkg ../input/boundary_segments_1320ft.gpkg | ../output
	$(R) create_permit_block_year_panel.R

../output/permit_block_year_panel_2015.parquet \
../output/permit_block_year_panel_2023.parquet \
../output/permit_block_year_panel_support_by_event_time.csv \
../output/permit_block_year_panel_support_by_calendar_time.csv \
../output/permit_block_year_panel_assignment_stability.csv \
../output/permit_block_year_panel_zero_share_summary.csv \
../output/permit_block_year_panel_outcome_totals.csv \
../output/permit_block_assignment_missing_2010.csv \
../output/permit_block_assignment_missing_2010_summary.csv \
../output/permit_block_assignment_missing_2020.csv \
../output/permit_block_assignment_missing_2020_summary.csv \
../output/permit_block_assignment_review_log.csv \
../output/permit_block_assignment_review_summary.csv \
../output/permit_unit_increase_audit.csv \
../output/permit_unit_increase_audit_summary.csv: ../output/permit_block_year_panel.parquet
	test -f "$@"

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	ln -sf "$<" "$@"

../input/building_permits_text_features.csv.gz: ../../building_permits_scraping/output/building_permits_text_features.csv.gz | ../input
	ln -sf "$<" "$@"

../input/block_treatment_panel.csv: ../../merge_event_study_scores/output/block_treatment_panel.csv | ../input
	ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf "$<" "$@"

../input/census_blocks_2020.csv: ../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/building_permits_clean.gpkg ../input/building_permits_text_features.csv.gz ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/ward_panel.gpkg ../input/boundary_segments_1320ft.gpkg
