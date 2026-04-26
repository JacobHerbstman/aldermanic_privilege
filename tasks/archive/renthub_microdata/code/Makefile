SHELL := bash

all: \
	../output/renthub_clean_rows.parquet \
	../output/renthub_listing_episodes.parquet \
	../output/renthub_repeat_pairs.parquet \
	../output/renthub_microdata_key_coverage.csv \
	../output/renthub_microdata_episode_diagnostics.csv \
	../output/renthub_microdata_repeat_pair_diagnostics.csv \
	../output/renthub_microdata_geography_coverage.csv \
	../output/renthub_microdata_trend_validation.csv

../output/renthub_clean_rows.parquet: build_renthub_microdata.R ../../_lib/aggregate_geography_helpers.R ../../_lib/renthub_market_helpers.R ../../_lib/renthub_raw_listing_helpers.R ../input/renthub_raw ../input/fred_chi_cpi_all_items.csv ../input/ward_panel.gpkg ../input/community_areas.geojson ../input/renthub_trim_sensitivity_summary.csv ../input/rent_with_ward_distances_full.parquet ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/renthub_market_citywide_series.csv | ../output ../temp
	Rscript build_renthub_microdata.R ../input/renthub_raw ../input/fred_chi_cpi_all_items.csv ../input/ward_panel.gpkg ../input/community_areas.geojson ../input/renthub_trim_sensitivity_summary.csv ../input/rent_with_ward_distances_full.parquet ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/renthub_market_citywide_series.csv ../output/renthub_clean_rows.parquet ../output/renthub_listing_episodes.parquet ../output/renthub_repeat_pairs.parquet ../output/renthub_microdata_key_coverage.csv ../output/renthub_microdata_episode_diagnostics.csv ../output/renthub_microdata_repeat_pair_diagnostics.csv ../output/renthub_microdata_geography_coverage.csv ../output/renthub_microdata_trend_validation.csv ../temp

../output/renthub_listing_episodes.parquet \
../output/renthub_repeat_pairs.parquet \
../output/renthub_microdata_key_coverage.csv \
../output/renthub_microdata_episode_diagnostics.csv \
../output/renthub_microdata_repeat_pair_diagnostics.csv \
../output/renthub_microdata_geography_coverage.csv \
../output/renthub_microdata_trend_validation.csv: ../output/renthub_clean_rows.parquet
	test -f $@

../input/renthub_raw: ../../download_rent_data/output | ../input
	ln -sfn $< $@

../input/fred_chi_cpi_all_items.csv: ../../process_dwellsy/input/fred_chi_cpi_all_items.csv | ../input
	ln -sfn $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sfn $< $@

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	ln -sfn $< $@

../input/renthub_trim_sensitivity_summary.csv: ../../compare_renthub_dwellsy_aggregate/output/renthub_trim_sensitivity_summary.csv | ../input
	ln -sfn $< $@

../input/rent_with_ward_distances_full.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sfn $< $@

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sfn $< $@

../input/census_blocks_2020.csv: ../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	ln -sfn $< $@

../input/renthub_market_citywide_series.csv: ../../renthub_market_index/output/renthub_market_citywide_series.csv | ../input
	ln -sfn $< $@

link-inputs: ../input/renthub_raw ../input/fred_chi_cpi_all_items.csv ../input/ward_panel.gpkg ../input/community_areas.geojson ../input/renthub_trim_sensitivity_summary.csv ../input/rent_with_ward_distances_full.parquet ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/renthub_market_citywide_series.csv

include ../../generic.make
