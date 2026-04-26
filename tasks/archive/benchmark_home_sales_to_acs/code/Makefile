SHELL := bash

all: \
	../output/home_sales_acs_citywide_index_2014_2019.pdf \
	../output/home_sales_acs_scatter_ward_2015_2019.pdf \
	../output/home_sales_acs_scatter_community_area_2014_2019.pdf \
	../output/home_sales_acs_map_ward_2015_2019.pdf \
	../output/home_sales_acs_map_community_area_2014_2019.pdf

../output/home_sales_acs_yearly_panel.csv ../output/home_sales_acs_growth_comparison.csv ../output/home_sales_acs_diagnostics.csv ../output/home_sales_acs_coverage_summary.csv: build_benchmark_home_sales_to_acs.R ../input/home_sales_geography_yearly_summary.csv ../input/ward_controls_2000_2023.csv ../input/block_group_controls_2000_2023.csv ../input/block_group_geometry_2019.gpkg ../input/community_areas.geojson ../input/fred_chi_cpi_all_items.csv | ../output
	Rscript build_benchmark_home_sales_to_acs.R ../input/home_sales_geography_yearly_summary.csv ../input/ward_controls_2000_2023.csv ../input/block_group_controls_2000_2023.csv ../input/block_group_geometry_2019.gpkg ../input/community_areas.geojson ../input/fred_chi_cpi_all_items.csv ../output/home_sales_acs_yearly_panel.csv ../output/home_sales_acs_growth_comparison.csv ../output/home_sales_acs_diagnostics.csv ../output/home_sales_acs_coverage_summary.csv

../output/home_sales_acs_citywide_index_2014_2019.pdf \
../output/home_sales_acs_scatter_ward_2015_2019.pdf \
../output/home_sales_acs_scatter_community_area_2014_2019.pdf \
../output/home_sales_acs_map_ward_2015_2019.pdf \
../output/home_sales_acs_map_community_area_2014_2019.pdf: plot_benchmark_home_sales_to_acs.R ../output/home_sales_acs_yearly_panel.csv ../output/home_sales_acs_growth_comparison.csv ../input/ward_panel.gpkg ../input/community_areas.geojson | ../output
	Rscript plot_benchmark_home_sales_to_acs.R ../output/home_sales_acs_yearly_panel.csv ../output/home_sales_acs_growth_comparison.csv ../input/ward_panel.gpkg ../input/community_areas.geojson ../output/home_sales_acs_citywide_index_2014_2019.pdf ../output/home_sales_acs_scatter_ward_2015_2019.pdf ../output/home_sales_acs_scatter_community_area_2014_2019.pdf ../output/home_sales_acs_map_ward_2015_2019.pdf ../output/home_sales_acs_map_community_area_2014_2019.pdf

../input/home_sales_geography_yearly_summary.csv: ../../aggregate_home_sales/output/home_sales_geography_yearly_summary.csv | ../input
	ln -sf "$<" "$@"

../input/ward_controls_2000_2023.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	ln -sf "$<" "$@"

../input/block_group_controls_2000_2023.csv: ../../create_ward_controls/output/block_group_controls_2000_2023.csv | ../input
	ln -sf "$<" "$@"

../input/block_group_geometry_2019.gpkg: ../../create_block_group_controls/output/block_group_geometry_2019.gpkg | ../input
	ln -sf "$<" "$@"

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	ln -sf "$<" "$@"

../input/fred_chi_cpi_all_items.csv: ../../process_dwellsy/input/fred_chi_cpi_all_items.csv | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/home_sales_geography_yearly_summary.csv ../input/ward_controls_2000_2023.csv ../input/block_group_controls_2000_2023.csv ../input/block_group_geometry_2019.gpkg ../input/community_areas.geojson ../input/fred_chi_cpi_all_items.csv ../input/ward_panel.gpkg

include ../../generic.make
