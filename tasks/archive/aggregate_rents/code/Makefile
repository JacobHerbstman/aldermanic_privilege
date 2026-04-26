SHELL := bash

all: \
	../output/rent_citywide_yoy_monthly.csv \
	../output/rent_citywide_yoy_longrun.pdf \
	../output/rent_citywide_yoy_recent.pdf \
	../output/rent_selected_areas_recent_levels.png \
	../output/rent_selected_areas_recent_yoy.png \
	../output/rent_selected_areas_index_2021.png \
	../output/rent_selected_areas_period_summary.csv \
	../output/rent_map_ward_2023_vs_2022_full_year.pdf \
	../output/rent_map_community_area_2023_vs_2022_full_year.pdf \
	../output/rent_map_ward_2024_vs_2023_full_year.pdf \
	../output/rent_map_community_area_2024_vs_2023_full_year.pdf \
	../output/rent_map_ward_2025_vs_2024_ytd.pdf \
	../output/rent_map_community_area_2025_vs_2024_ytd.pdf \
	../output/rent_map_ward_2026_vs_2025_ytd.pdf \
	../output/rent_map_community_area_2026_vs_2025_ytd.pdf

../output/rent_geography_monthly_summary.csv ../output/rent_geography_yearly_summary.csv ../output/rent_map_period_summary.csv ../output/rent_coverage_summary.csv: build_aggregate_rents.R ../input/chicago_rent_panel.parquet ../input/chicago_dwellsy_listing_history.parquet ../input/fred_chi_cpi_all_items.csv ../input/ward_panel.gpkg ../input/community_areas.geojson | ../output ../temp
	Rscript build_aggregate_rents.R ../input/chicago_rent_panel.parquet ../input/chicago_dwellsy_listing_history.parquet ../input/fred_chi_cpi_all_items.csv ../input/ward_panel.gpkg ../input/community_areas.geojson ../input/renthub_trim_sensitivity_summary.csv ../output/rent_geography_monthly_summary.csv ../output/rent_geography_yearly_summary.csv ../output/rent_map_period_summary.csv ../output/rent_coverage_summary.csv

../output/rent_citywide_yoy_monthly.csv \
../output/rent_citywide_yoy_longrun.pdf \
../output/rent_citywide_yoy_recent.pdf \
../output/rent_map_ward_2023_vs_2022_full_year.pdf \
../output/rent_map_community_area_2023_vs_2022_full_year.pdf \
../output/rent_map_ward_2024_vs_2023_full_year.pdf \
../output/rent_map_community_area_2024_vs_2023_full_year.pdf \
../output/rent_map_ward_2025_vs_2024_ytd.pdf \
../output/rent_map_community_area_2025_vs_2024_ytd.pdf \
../output/rent_map_ward_2026_vs_2025_ytd.pdf \
../output/rent_map_community_area_2026_vs_2025_ytd.pdf: plot_aggregate_rents.R ../output/rent_geography_monthly_summary.csv ../output/rent_map_period_summary.csv ../input/ward_panel.gpkg ../input/community_areas.geojson | ../output
	Rscript plot_aggregate_rents.R ../output/rent_geography_monthly_summary.csv ../output/rent_map_period_summary.csv ../input/ward_panel.gpkg ../input/community_areas.geojson ../output/rent_citywide_yoy_monthly.csv ../output

../output/rent_selected_areas_recent_levels.png ../output/rent_selected_areas_recent_yoy.png ../output/rent_selected_areas_index_2021.png ../output/rent_selected_areas_period_summary.csv: plot_selected_areas_rents.R ../output/rent_geography_monthly_summary.csv ../output/rent_map_period_summary.csv | ../output
	Rscript plot_selected_areas_rents.R ../output/rent_geography_monthly_summary.csv ../output/rent_map_period_summary.csv ../output/rent_selected_areas_recent_levels.png ../output/rent_selected_areas_recent_yoy.png ../output/rent_selected_areas_index_2021.png ../output/rent_selected_areas_period_summary.csv

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	ln -sf "$<" "$@"

../input/chicago_dwellsy_listing_history.parquet: ../../process_dwellsy/output/chicago_dwellsy_listing_history.parquet | ../input
	ln -sf "$<" "$@"

../input/fred_chi_cpi_all_items.csv: ../../process_dwellsy/input/fred_chi_cpi_all_items.csv | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/chicago_rent_panel.parquet ../input/chicago_dwellsy_listing_history.parquet ../input/fred_chi_cpi_all_items.csv ../input/ward_panel.gpkg ../input/community_areas.geojson
	if [ -f ../../compare_renthub_dwellsy_aggregate/output/renthub_trim_sensitivity_summary.csv ]; then \
		ln -sf ../../compare_renthub_dwellsy_aggregate/output/renthub_trim_sensitivity_summary.csv ../input/renthub_trim_sensitivity_summary.csv; \
	fi

include ../../generic.make
