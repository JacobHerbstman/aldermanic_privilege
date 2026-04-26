SHELL := bash
include ../../shell_functions.make

all: \
	../output/.process_dwellsy_complete

../output/.process_dwellsy_complete: process_dwellsy.R ../input/dwellsy_raw/.linked ../input/chicago_boundary.geojson ../input/zillow_city_zori.csv ../input/zillow_metro_zori.csv ../input/fred_chi_rent_cpi.csv ../input/fred_chi_cpi_all_items.csv | ../output ../temp
	$(R) process_dwellsy.R ../input/dwellsy_raw ../input/chicago_boundary.geojson ../input/zillow_city_zori.csv ../input/zillow_metro_zori.csv ../input/fred_chi_rent_cpi.csv ../input/fred_chi_cpi_all_items.csv ../output ../temp
	test -f ../output/chicago_dwellsy_listing_history.parquet
	test -f ../output/chicago_dwellsy_listing_analysis_sample.parquet
	test -f ../output/dwellsy_processing_sample_ladder.csv
	test -f ../output/dwellsy_quality_summary.csv
	test -f ../output/dwellsy_duplicate_listing_issues.csv
	test -f ../output/dwellsy_coordinate_issues.csv
	test -f ../output/dwellsy_id_integrity.csv
	test -f ../output/dwellsy_repeat_listing_resolution.csv
	test -f ../output/dwellsy_value_integrity_issues.csv
	test -f ../output/dwellsy_amenity_indicators.parquet
	test -f ../output/dwellsy_amenity_dictionary.csv
	test -f ../output/dwellsy_monthly_series.csv
	test -f ../output/dwellsy_monthly_series_variants.csv
	test -f ../output/dwellsy_benchmark_comparison.csv
	test -f ../output/dwellsy_benchmark_scorecard.csv
	test -f ../output/dwellsy_benchmark_comparison_real.csv
	test -f ../output/dwellsy_benchmark_scorecard_real.csv
	test -f ../output/dwellsy_quality_memo.md
	touch $@

../input/dwellsy_raw:
	mkdir -p $@

../input/dwellsy_raw/.linked: | ../input/dwellsy_raw
	for f in ../../../data_raw/dwellsy/*.parquet; do \
		ln -sfn "$$(cd "$$(dirname "$$f")" && pwd)/$$(basename "$$f")" ../input/dwellsy_raw/$$(basename "$$f"); \
	done
	touch $@

../input/chicago_boundary.geojson: ../../../data_raw/Boundaries_-_City_20250920.geojson | ../input
	ln -sfn $(abspath $<) $@

../input/zillow_city_zori.csv: | ../input
	curl -sL 'https://files.zillowstatic.com/research/public_csvs/zori/City_zori_uc_sfrcondomfr_sm_month.csv' -o $@

../input/zillow_metro_zori.csv: | ../input
	curl -sL 'https://files.zillowstatic.com/research/public_csvs/zori/Metro_zori_uc_sfrcondomfr_sm_month.csv' -o $@

../input/fred_chi_rent_cpi.csv: | ../input
	curl -sL 'https://fred.stlouisfed.org/graph/fredgraph.csv?id=CUURA207SEHA' -o $@

../input/fred_chi_cpi_all_items.csv: | ../input
	curl -sL 'https://fred.stlouisfed.org/graph/fredgraph.csv?id=CUURA207SA0' -o $@

link-inputs: ../input/dwellsy_raw/.linked ../input/chicago_boundary.geojson ../input/zillow_city_zori.csv ../input/zillow_metro_zori.csv ../input/fred_chi_rent_cpi.csv ../input/fred_chi_cpi_all_items.csv

include ../../generic.make
