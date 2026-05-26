SHELL := bash

all: \
	../output/price_rd_stress_test_estimates.csv \
	../output/price_rd_supply_first_stage_segments.csv \
	../output/price_rd_supply_first_stage_support.csv \
	../output/price_rd_segment_context_splits.csv \
	../output/price_rd_stress_test_support.csv \
	../output/price_rd_old_stock_attrition.csv \
	../output/price_rd_pruned_sample_support.csv \
	../output/listed_supply_normalization_diagnostics.csv \
	../output/price_rd_supply_split_estimates.tex \
	../output/price_rd_supply_split_estimates.pdf \
	../output/price_rd_stress_test_estimates.pdf

../output/price_rd_stress_test_estimates.csv: build_price_rd_stress_tests.R ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/parcels_with_ward_distances.csv ../input/confounded_segment_flags.csv ../input/residential_cross_section.csv ../input/parcel_segment_ids.csv | ../output
	$(R) $<

../output/price_rd_supply_first_stage_segments.csv \
../output/price_rd_supply_first_stage_support.csv \
../output/price_rd_segment_context_splits.csv \
../output/price_rd_stress_test_support.csv \
../output/price_rd_old_stock_attrition.csv \
../output/price_rd_pruned_sample_support.csv \
../output/listed_supply_normalization_diagnostics.csv \
../output/price_rd_supply_split_estimates.tex \
../output/price_rd_supply_split_estimates.pdf \
../output/price_rd_stress_test_estimates.pdf: ../output/price_rd_stress_test_estimates.csv
	@test -s "$@"

../input/rental_rd_characteristics_panel_bw500.parquet: ../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/confounded_segment_flags.csv: ../../../border_confound_pruning/output/confounded_segment_flags.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/residential_cross_section.csv: ../../../residential_improvements_data_cleaning/output/residential_cross_section.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/parcels_with_ward_distances.csv ../input/confounded_segment_flags.csv ../input/residential_cross_section.csv ../input/parcel_segment_ids.csv

include ../../../shell_functions.make
include ../../../generic.make
