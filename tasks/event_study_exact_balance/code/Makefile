SHELL := bash
include ../../shell_functions.make

all: ../output/permit_exact_balance_summary.tex ../output/sales_exact_balance_summary.tex

../output/block_parcel_baselines_2014.csv: build_block_parcel_baselines_2014.R ../input/geocoded_residential_data.gpkg ../input/zoning_data_clean.gpkg ../input/census_blocks_2010.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp | ../output
	Rscript build_block_parcel_baselines_2014.R ../input/geocoded_residential_data.gpkg ../input/zoning_data_clean.gpkg ../input/census_blocks_2010.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../output/block_parcel_baselines_2014.csv

../output/permit_exact_balance_summary.csv ../output/permit_exact_balance_summary.tex ../output/sales_exact_balance_summary.csv ../output/sales_exact_balance_summary.tex: build_event_study_exact_balance.R ../output/block_parcel_baselines_2014.csv ../input/permit_block_year_panel_2015.parquet ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript build_event_study_exact_balance.R ../input/permit_block_year_panel_2015.parquet ../input/sales_transaction_panel_2015.parquet ../output/block_parcel_baselines_2014.csv ../output/permit_exact_balance_summary.csv ../output/permit_exact_balance_summary.tex ../output/sales_exact_balance_summary.csv ../output/sales_exact_balance_summary.tex

../input/geocoded_residential_data.gpkg: ../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	ln -sf "$<" "$@"

../input/zoning_data_clean.gpkg: ../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
	ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../create_event_study_permit_data/input/census_blocks_2010.csv | ../input
	ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../download_amenities_data/output/schools_2015.gpkg | ../input
	ln -sf "$<" "$@"

../input/parks.gpkg: ../../download_amenities_data/output/parks.gpkg | ../input
	ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../download_amenities_data/output/major_streets.gpkg | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	ln -sf ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.* $(dir $@)

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/geocoded_residential_data.gpkg ../input/zoning_data_clean.gpkg ../input/census_blocks_2010.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/permit_block_year_panel_2015.parquet ../input/sales_transaction_panel_2015.parquet

include ../../generic.make
