SHELL := bash

EVENT_STUDY_BW_M ?= 304.8
EVENT_STUDY_BW_LABEL ?= 1000ft

all: \
	../output/permit_exact_balance_summary_$(EVENT_STUDY_BW_LABEL).tex \
	../output/sales_exact_balance_summary_$(EVENT_STUDY_BW_LABEL).tex \
	../output/redistricting_sample_support_summary_$(EVENT_STUDY_BW_LABEL).tex

../output/block_parcel_baselines_2014.csv: build_block_parcel_baselines_2014.R ../input/geocoded_residential_data.gpkg ../input/zoning_data_clean.gpkg ../input/census_blocks_2010.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript build_block_parcel_baselines_2014.R ../input/geocoded_residential_data.gpkg ../input/zoning_data_clean.gpkg ../input/census_blocks_2010.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../output/block_parcel_baselines_2014.csv

../output/permit_exact_balance_summary_$(EVENT_STUDY_BW_LABEL).csv: build_event_study_exact_balance.R ../output/block_parcel_baselines_2014.csv ../input/permit_block_year_panel_2015.parquet ../input/sales_transaction_panel_2015.parquet | ../output
	BANDWIDTH_LABEL=$(EVENT_STUDY_BW_LABEL) Rscript build_event_study_exact_balance.R ../input/permit_block_year_panel_2015.parquet ../input/sales_transaction_panel_2015.parquet ../output/block_parcel_baselines_2014.csv ../output/permit_exact_balance_summary_$(EVENT_STUDY_BW_LABEL).csv ../output/permit_exact_balance_summary_$(EVENT_STUDY_BW_LABEL).tex ../output/sales_exact_balance_summary_$(EVENT_STUDY_BW_LABEL).csv ../output/sales_exact_balance_summary_$(EVENT_STUDY_BW_LABEL).tex $(EVENT_STUDY_BW_M)

../output/permit_exact_balance_summary_$(EVENT_STUDY_BW_LABEL).tex ../output/sales_exact_balance_summary_$(EVENT_STUDY_BW_LABEL).csv ../output/sales_exact_balance_summary_$(EVENT_STUDY_BW_LABEL).tex: ../output/permit_exact_balance_summary_$(EVENT_STUDY_BW_LABEL).csv

../output/redistricting_sample_support_summary_$(EVENT_STUDY_BW_LABEL).csv: build_redistricting_sample_support.R ../input/permit_block_year_panel_2015.parquet ../input/sales_transaction_panel_2015.parquet ../output/block_parcel_baselines_2014.csv | ../output
	BANDWIDTH_LABEL=$(EVENT_STUDY_BW_LABEL) Rscript build_redistricting_sample_support.R ../input/permit_block_year_panel_2015.parquet ../input/sales_transaction_panel_2015.parquet ../output/block_parcel_baselines_2014.csv ../output/redistricting_sample_support_summary_$(EVENT_STUDY_BW_LABEL).csv ../output/redistricting_sample_support_summary_$(EVENT_STUDY_BW_LABEL).tex $(EVENT_STUDY_BW_M)

../output/redistricting_sample_support_summary_$(EVENT_STUDY_BW_LABEL).tex: ../output/redistricting_sample_support_summary_$(EVENT_STUDY_BW_LABEL).csv

../input/geocoded_residential_data.gpkg: ../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/zoning_data_clean.gpkg: ../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../download_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../download_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../download_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg: ../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/geocoded_residential_data.gpkg ../input/zoning_data_clean.gpkg ../input/census_blocks_2010.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg ../input/permit_block_year_panel_2015.parquet ../input/sales_transaction_panel_2015.parquet

include ../../shell_functions.make
include ../../generic.make
