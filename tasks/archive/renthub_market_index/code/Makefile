SHELL := bash

all: \
	../output/renthub_market_citywide_series.csv \
	../output/renthub_market_benchmark_comparison.csv \
	../output/renthub_market_benchmark_summary.csv \
	../output/renthub_market_neighborhood_monthly.csv \
	../output/renthub_market_neighborhood_periods.csv \
	../output/renthub_market_coverage_summary.csv \
	../output/renthub_market_acs_growth_comparison.csv \
	../output/renthub_market_acs_diagnostics.csv \
	../output/renthub_market_citywide_smooth_index.pdf \
	../output/renthub_market_citywide_asking_index.pdf \
	../output/renthub_market_citywide_yoy_recent.pdf \
	../output/renthub_market_map_ward_2023_vs_2022.pdf \
	../output/renthub_market_map_community_area_2023_vs_2022.pdf \
	../output/renthub_market_map_ward_2024_vs_2023.pdf \
	../output/renthub_market_map_community_area_2024_vs_2023.pdf \
	../output/renthub_market_map_ward_2025_vs_2024_ytd.pdf \
	../output/renthub_market_map_community_area_2025_vs_2024_ytd.pdf

../output/renthub_market_citywide_series.csv: build_renthub_market_index.R ../../_lib/aggregate_geography_helpers.R ../../_lib/acs_benchmark_helpers.R ../../_lib/renthub_market_helpers.R ../../_lib/renthub_raw_listing_helpers.R ../input/renthub_raw ../input/external_benchmark_comparison.csv ../input/fred_chi_cpi_all_items.csv ../input/ward_panel.gpkg ../input/community_areas.geojson ../input/ward_controls_2000_2023.csv ../input/block_group_controls_2000_2023.csv ../input/block_group_geometry_2019.gpkg ../input/renthub_trim_sensitivity_summary.csv | ../output ../temp
	Rscript build_renthub_market_index.R ../input/renthub_raw ../input/external_benchmark_comparison.csv ../input/fred_chi_cpi_all_items.csv ../input/ward_panel.gpkg ../input/community_areas.geojson ../input/ward_controls_2000_2023.csv ../input/block_group_controls_2000_2023.csv ../input/block_group_geometry_2019.gpkg ../input/renthub_trim_sensitivity_summary.csv ../output/renthub_market_citywide_series.csv ../output/renthub_market_benchmark_comparison.csv ../output/renthub_market_benchmark_summary.csv ../output/renthub_market_neighborhood_monthly.csv ../output/renthub_market_neighborhood_periods.csv ../output/renthub_market_coverage_summary.csv ../output/renthub_market_acs_growth_comparison.csv ../output/renthub_market_acs_diagnostics.csv ../temp

../output/renthub_market_benchmark_comparison.csv \
../output/renthub_market_benchmark_summary.csv \
../output/renthub_market_neighborhood_monthly.csv \
../output/renthub_market_neighborhood_periods.csv \
../output/renthub_market_coverage_summary.csv \
../output/renthub_market_acs_growth_comparison.csv \
../output/renthub_market_acs_diagnostics.csv: ../output/renthub_market_citywide_series.csv
	test -f $@

../output/renthub_market_citywide_smooth_index.pdf: plot_renthub_market_index.R ../output/renthub_market_benchmark_comparison.csv ../output/renthub_market_neighborhood_periods.csv ../input/ward_panel.gpkg ../input/community_areas.geojson | ../output
	Rscript plot_renthub_market_index.R ../output/renthub_market_benchmark_comparison.csv ../output/renthub_market_neighborhood_periods.csv ../input/ward_panel.gpkg ../input/community_areas.geojson ../output/renthub_market_citywide_smooth_index.pdf ../output/renthub_market_citywide_asking_index.pdf ../output/renthub_market_citywide_yoy_recent.pdf ../output/renthub_market_map_ward_2023_vs_2022.pdf ../output/renthub_market_map_community_area_2023_vs_2022.pdf ../output/renthub_market_map_ward_2024_vs_2023.pdf ../output/renthub_market_map_community_area_2024_vs_2023.pdf ../output/renthub_market_map_ward_2025_vs_2024_ytd.pdf ../output/renthub_market_map_community_area_2025_vs_2024_ytd.pdf

../output/renthub_market_citywide_asking_index.pdf \
../output/renthub_market_citywide_yoy_recent.pdf \
../output/renthub_market_map_ward_2023_vs_2022.pdf \
../output/renthub_market_map_community_area_2023_vs_2022.pdf \
../output/renthub_market_map_ward_2024_vs_2023.pdf \
../output/renthub_market_map_community_area_2024_vs_2023.pdf \
../output/renthub_market_map_ward_2025_vs_2024_ytd.pdf \
../output/renthub_market_map_community_area_2025_vs_2024_ytd.pdf: ../output/renthub_market_citywide_smooth_index.pdf
	test -f $@

../input/renthub_raw: ../../download_rent_data/output | ../input
	ln -sfn $< $@

../input/external_benchmark_comparison.csv: ../../renthub_data_audit/output/external_benchmark_comparison.csv | ../input
	ln -sfn $< $@

../input/fred_chi_cpi_all_items.csv: ../../process_dwellsy/input/fred_chi_cpi_all_items.csv | ../input
	ln -sfn $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sfn $< $@

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	ln -sfn $< $@

../input/ward_controls_2000_2023.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	ln -sfn $< $@

../input/block_group_controls_2000_2023.csv: ../../create_ward_controls/output/block_group_controls_2000_2023.csv | ../input
	ln -sfn $< $@

../input/block_group_geometry_2019.gpkg: ../../create_block_group_controls/output/block_group_geometry_2019.gpkg | ../input
	ln -sfn $< $@

../input/renthub_trim_sensitivity_summary.csv: ../../compare_renthub_dwellsy_aggregate/output/renthub_trim_sensitivity_summary.csv | ../input
	ln -sfn $< $@

link-inputs: ../input/renthub_raw ../input/external_benchmark_comparison.csv ../input/fred_chi_cpi_all_items.csv ../input/ward_panel.gpkg ../input/community_areas.geojson ../input/ward_controls_2000_2023.csv ../input/block_group_controls_2000_2023.csv ../input/block_group_geometry_2019.gpkg ../input/renthub_trim_sensitivity_summary.csv

include ../../generic.make
