SHELL := bash

all: \
	../output/home_sales_citywide_yoy_monthly.csv \
	../output/home_sales_citywide_yoy_annual.csv \
	../output/home_sales_citywide_yoy_annual_longrun.pdf \
	../output/home_sales_citywide_yoy_recent_monthly.pdf \
	../output/home_sales_citywide_transaction_yoy_recent_monthly.pdf \
	../output/home_sales_selected_areas_recent_yoy.png \
	../output/home_sales_selected_areas_annual_levels.png \
	../output/home_sales_selected_areas_period_summary.csv \
	../output/home_sales_map_ward_2023_vs_2022_full_year.pdf \
	../output/home_sales_map_community_area_2023_vs_2022_full_year.pdf \
	../output/home_sales_map_ward_2024_vs_2023_full_year.pdf \
	../output/home_sales_map_community_area_2024_vs_2023_full_year.pdf \
	../output/home_sales_map_ward_2025_vs_2024_ytd.pdf \
	../output/home_sales_map_community_area_2025_vs_2024_ytd.pdf

../output/home_sales_geography_monthly_summary.csv ../output/home_sales_geography_yearly_summary.csv ../output/home_sales_map_period_summary.csv ../output/home_sales_coverage_summary.csv: build_aggregate_home_sales.R ../input/sales_with_hedonics.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson | ../output
	Rscript build_aggregate_home_sales.R ../input/sales_with_hedonics.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson ../output/home_sales_geography_monthly_summary.csv ../output/home_sales_geography_yearly_summary.csv ../output/home_sales_map_period_summary.csv ../output/home_sales_coverage_summary.csv

../output/home_sales_citywide_yoy_monthly.csv \
../output/home_sales_citywide_yoy_annual.csv \
../output/home_sales_citywide_yoy_annual_longrun.pdf \
../output/home_sales_citywide_yoy_recent_monthly.pdf \
../output/home_sales_citywide_transaction_yoy_recent_monthly.pdf \
../output/home_sales_map_ward_2023_vs_2022_full_year.pdf \
../output/home_sales_map_community_area_2023_vs_2022_full_year.pdf \
../output/home_sales_map_ward_2024_vs_2023_full_year.pdf \
../output/home_sales_map_community_area_2024_vs_2023_full_year.pdf \
../output/home_sales_map_ward_2025_vs_2024_ytd.pdf \
../output/home_sales_map_community_area_2025_vs_2024_ytd.pdf: plot_aggregate_home_sales.R ../output/home_sales_geography_monthly_summary.csv ../output/home_sales_geography_yearly_summary.csv ../output/home_sales_map_period_summary.csv ../input/ward_panel.gpkg ../input/community_areas.geojson | ../output
	Rscript plot_aggregate_home_sales.R ../output/home_sales_geography_monthly_summary.csv ../output/home_sales_geography_yearly_summary.csv ../output/home_sales_map_period_summary.csv ../input/ward_panel.gpkg ../input/community_areas.geojson ../output/home_sales_citywide_yoy_monthly.csv ../output/home_sales_citywide_yoy_annual.csv ../output

../output/home_sales_selected_areas_recent_yoy.png ../output/home_sales_selected_areas_annual_levels.png ../output/home_sales_selected_areas_period_summary.csv: plot_selected_areas_home_sales.R ../output/home_sales_geography_monthly_summary.csv ../output/home_sales_geography_yearly_summary.csv ../output/home_sales_map_period_summary.csv | ../output
	Rscript plot_selected_areas_home_sales.R ../output/home_sales_geography_monthly_summary.csv ../output/home_sales_geography_yearly_summary.csv ../output/home_sales_map_period_summary.csv ../output/home_sales_selected_areas_recent_yoy.png ../output/home_sales_selected_areas_annual_levels.png ../output/home_sales_selected_areas_period_summary.csv

../input/sales_with_hedonics.parquet: ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/sales_with_hedonics.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson

include ../../generic.make
