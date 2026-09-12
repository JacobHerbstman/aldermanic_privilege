include ../../shared/code/shell_functions.make

MIN_CONSTRUCTION_YEAR ?= 1999
PREFERRED_ASSESSMENT_YEAR ?= 2022
FALLBACK_ASSESSMENT_YEAR ?= 2025

all: ../output/geocoded_residential_data.gpkg

../%/residential_cross_section.csv \
	../%/residential_discovery_cross_section.csv \
	../%/multifamily_data_cleaned.csv: prepare_construction_records.R \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/residential_assessor_history.parquet \
	../input/commercial_value_raw.csv \
	Makefile | ../output ../report
	$(R) $< $(MIN_CONSTRUCTION_YEAR) $(PREFERRED_ASSESSMENT_YEAR) $(FALLBACK_ASSESSMENT_YEAR)

../output/geocoded_residential_data.gpkg: geocode_residential_data.R \
	../output/residential_cross_section.csv \
	../output/multifamily_data_cleaned.csv \
	../input/density_historical_coordinates.csv \
	../input/parcel_universe_2025_city.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	Makefile | ../output ../report
	$(R) $<

../input/commercial_value_raw.csv: ../../../data_raw/construction_review/commercial_valuation_data.csv | ../input
	ln -sf $< $@

../input/density_historical_coordinates.csv: ../../construction_historical_coordinates/output/density_historical_coordinates.csv | ../input
	ln -sf $< $@

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	ln -sf $< $@

../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/commercial_value_raw.csv \
	../input/density_historical_coordinates.csv \
	../input/parcel_universe_2025_city.csv \
	../input/residential_assessor_history.parquet

include ../../shared/code/generic.make
