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
	../output/price_rd_stress_test_estimates.pdf \
	../output/rent_placebo_rd_main_style.png \
	../output/sales_placebo_rd_main_style.png \
	../output/rent_pruned_rd_main_style.png \
	../output/sales_pruned_rd_main_style.png \
	../output/rent_local_linear_rd_main_style.png \
	../output/sales_local_linear_rd_main_style.png \
	../output/rent_levels_rd_main_style.png \
	../output/sales_levels_rd_main_style.png

../output/price_rd_stress_test_estimates.csv: build_price_rd_stress_tests.R ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/parcels_with_ward_distances.csv ../input/confounded_segment_flags.csv ../input/confounded_pair_era_flags.csv ../input/residential_cross_section.csv ../input/parcel_segment_ids.csv | ../output
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

../output/rent_placebo_rd_main_style.png: build_price_rd_main_style_plots.R ../input/rent_with_ward_distances_full.parquet ../input/sales_with_hedonics_amenities.parquet ../input/confounded_segment_flags.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	$(R) $<

../output/rent_placebo_rd_main_style.pdf \
../output/sales_placebo_rd_main_style.png \
../output/sales_placebo_rd_main_style.pdf \
../output/rent_pruned_rd_main_style.png \
../output/rent_pruned_rd_main_style.pdf \
../output/sales_pruned_rd_main_style.png \
../output/sales_pruned_rd_main_style.pdf \
../output/rent_local_linear_rd_main_style.png \
../output/rent_local_linear_rd_main_style.pdf \
../output/sales_local_linear_rd_main_style.png \
../output/sales_local_linear_rd_main_style.pdf \
../output/rent_levels_rd_main_style.png \
../output/rent_levels_rd_main_style.pdf \
../output/sales_levels_rd_main_style.png \
../output/sales_levels_rd_main_style.pdf \
../output/price_rd_main_style_plot_estimates.csv \
../output/price_rd_main_style_plot_bins.csv: ../output/rent_placebo_rd_main_style.png
	@test -s "$@"

../input/rental_rd_characteristics_panel_bw500.parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rent_with_ward_distances_full.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/confounded_segment_flags.csv: ../../border_confound_pruning/output/confounded_segment_flags.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/residential_cross_section.csv: ../../residential_improvements_data_cleaning/output/residential_cross_section.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../prepare_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../prepare_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cta_stops.gpkg: ../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.dbf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.prj | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shx | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.cpg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw500.parquet ../input/rent_with_ward_distances_full.parquet ../input/sales_with_hedonics_amenities.parquet ../input/parcels_with_ward_distances.csv ../input/confounded_segment_flags.csv ../input/confounded_pair_era_flags.csv ../input/residential_cross_section.csv ../input/parcel_segment_ids.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../shell_functions.make
include ../../generic.make
