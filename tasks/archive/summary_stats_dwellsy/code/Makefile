SHELL := bash
include ../../shell_functions.make

all: \
	../output/dwellsy_summary_stats.csv \
	../output/dwellsy_block_group_map_data.csv \
	../output/dwellsy_block_group_rent_comparison.csv \
	../output/dwellsy_block_group_rent_comparison_summary.csv \
	../output/dwellsy_analysis_integrity_extremes.csv \
	../output/acs_block_group_rent_2024.csv \
	../output/fig_dwellsy_chicago_block_group_maps.pdf \
	../output/fig_dwellsy_distributions.pdf \
	../output/fig_dwellsy_block_group_rent_vs_acs.pdf

../output/dwellsy_summary_stats.csv \
../output/dwellsy_block_group_map_data.csv \
../output/dwellsy_block_group_rent_comparison.csv \
../output/dwellsy_block_group_rent_comparison_summary.csv \
../output/dwellsy_analysis_integrity_extremes.csv \
../output/acs_block_group_rent_2024.csv \
../output/fig_dwellsy_chicago_block_group_maps.pdf \
../output/fig_dwellsy_distributions.pdf \
../output/fig_dwellsy_block_group_rent_vs_acs.pdf &: run_summary_stats_dwellsy.R ../input/chicago_dwellsy_listing_analysis_sample.parquet ../input/block_group_geometry_2019.gpkg ../input/chicago_boundary.geojson | ../output
	$(R) run_summary_stats_dwellsy.R ../input/chicago_dwellsy_listing_analysis_sample.parquet ../input/block_group_geometry_2019.gpkg ../input/chicago_boundary.geojson ../output

../input/chicago_dwellsy_listing_analysis_sample.parquet: ../../process_dwellsy/output/chicago_dwellsy_listing_analysis_sample.parquet | ../input
	ln -sf $< $@

../input/block_group_geometry_2019.gpkg: ../../create_block_group_controls/output/block_group_geometry_2019.gpkg | ../input
	ln -sf $< $@

../input/chicago_boundary.geojson: ../../../data_raw/Boundaries_-_City_20250920.geojson | ../input
	ln -sfn $(abspath $<) $@

link-inputs: ../input/chicago_dwellsy_listing_analysis_sample.parquet ../input/block_group_geometry_2019.gpkg ../input/chicago_boundary.geojson

include ../../generic.make
