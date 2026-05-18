SHELL := bash

START_DATE ?= 2014-01-01
END_DATE ?= 2022-12-31

all: \
	../output/chicago_rent_panel_quality_flags.parquet \
	../output/address_coordinate_stability.csv \
	../output/address_location_group_stability.csv \
	../output/unstable_address_external_review_queue.csv \
	../output/unstable_address_external_review_workbook.xlsx \
	../output/property_proxy_stability.csv \
	../output/floorplan_same_day_rent_spread.csv \
	../output/coordinate_only_pile_audit.csv \
	../output/one_day_bulk_audit.csv \
	../output/building_type_conflict_audit.csv \
	../output/stale_posted_audit.csv \
	../output/renthub_quality_flag_summary.csv \
	../output/harrison_221_spotcheck.csv

../output/chicago_rent_panel_quality_flags.parquet: run_renthub_proxy_consistency_audit.R manual_verified_address_locations.csv ../input/renthub_raw ../input/chicago_rent_panel.parquet | ../output
	# arg order: start_date end_date
	$(R) $< $(START_DATE) $(END_DATE)

../output/address_coordinate_stability.csv \
../output/address_location_group_stability.csv \
../output/unstable_address_external_review_queue.csv \
../output/unstable_address_external_review_workbook.xlsx \
../output/property_proxy_stability.csv \
../output/floorplan_same_day_rent_spread.csv \
../output/coordinate_only_pile_audit.csv \
../output/one_day_bulk_audit.csv \
../output/building_type_conflict_audit.csv \
../output/stale_posted_audit.csv \
../output/renthub_quality_flag_summary.csv \
../output/harrison_221_spotcheck.csv: ../output/chicago_rent_panel_quality_flags.parquet
	@test -s "$@"

../input/renthub_raw: ../../download_rent_data/output/download.done | ../input
	@target="../../download_rent_data/output"; test "$$(readlink "$@" 2>/dev/null)" = "$$target" || ln -sfn "$$target" "$@"

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/renthub_raw ../input/chicago_rent_panel.parquet

include ../../shell_functions.make
include ../../generic.make
