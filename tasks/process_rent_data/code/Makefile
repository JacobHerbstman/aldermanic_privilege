SHELL := bash

START_DATE ?= 2014-01-01
END_DATE ?= 2022-12-31

all: \
	../output/chicago_rent_panel.parquet \
	../output/chicago_rent_panel_drop_multi_rent.parquet \
	../output/chicago_rent_episode_robustness.parquet \
	../output/renthub_processing_diagnostics.csv \
	../output/renthub_episode_diagnostics.csv \
	../output/renthub_monthly_series.csv \
	../output/renthub_key_diagnostics.csv \
	../output/renthub_building_type_diagnostics.csv \
	../output/renthub_property_day_diagnostics.csv \
	../output/renthub_floorplan_day_diagnostics.csv \
	../output/renthub_floorplan_month_diagnostics.csv \
	../output/renthub_collision_examples.csv \
	../output/renthub_drop_reasons.csv \
	../output/renthub_drop_reasons_by_year.csv \
	../output/renthub_city_filter_diagnostics.csv

../output/chicago_rent_panel.parquet: build_renthub_clean_panel.R ../input/renthub_raw | ../output
	# arg order: start_date end_date
	$(R) $< $(START_DATE) $(END_DATE)

../output/chicago_rent_panel_drop_multi_rent.parquet \
../output/chicago_rent_episode_robustness.parquet \
../output/renthub_processing_diagnostics.csv \
../output/renthub_episode_diagnostics.csv \
../output/renthub_monthly_series.csv \
../output/renthub_key_diagnostics.csv \
../output/renthub_building_type_diagnostics.csv \
../output/renthub_property_day_diagnostics.csv \
../output/renthub_floorplan_day_diagnostics.csv \
../output/renthub_floorplan_month_diagnostics.csv \
../output/renthub_collision_examples.csv \
../output/renthub_drop_reasons.csv \
../output/renthub_drop_reasons_by_year.csv \
../output/renthub_city_filter_diagnostics.csv: ../output/chicago_rent_panel.parquet
	@test -s "$@"

../input/renthub_raw: ../../download_rent_data/output/download.done | ../input
	@target="../../download_rent_data/output"; test "$$(readlink "$@" 2>/dev/null)" = "$$target" || ln -sfn "$$target" "$@"

link-inputs: ../input/renthub_raw

include ../../shell_functions.make
include ../../generic.make
