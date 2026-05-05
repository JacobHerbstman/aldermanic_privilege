SHELL := bash
include ../../shell_functions.make

PARCEL_YEAR := 2025
TRIAD_NAME := City

all: ../output/parcel_universe_2025_city.csv

../temp/parcel_universe_2025_city_native.csv: download_parcel_universe_data.sh | ../temp ../output
	bash $< $(PARCEL_YEAR) $(TRIAD_NAME) $@ ../output/parcel_universe_2025_city_metadata.csv

../output/parcel_universe_2025_city.csv: rewrite_parcel_universe_schema.R ../temp/parcel_universe_2025_city_native.csv | ../output
	$(R) $< ../temp/parcel_universe_2025_city_native.csv $@

include ../../generic.make
