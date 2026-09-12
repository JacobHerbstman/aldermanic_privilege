include ../../shared/code/shell_functions.make

PARCEL_YEAR := 2025
TRIAD_NAME := City

all: ../output/parcel_universe_2025_city.csv

download-current: ../output/parcel_universe_2025_city_native_current.csv

../output/parcel_universe_2025_city.csv: rewrite_parcel_universe_schema.R ../../shared/code/save_data.R ../../shared/code/report.py ../temp/parcel_universe_2025_city_native.csv ../../setup_environment/code/packages.R | ../output ../report
	$(R) $<

../temp/parcel_universe_2025_city_native.csv: ../../../data_raw/construction_review/parcel_universe_2025_city_native.csv parcel_universe_snapshot.sha256 Makefile | ../temp
	shasum -a 256 -c parcel_universe_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

../output/parcel_universe_2025_city_native_current.csv: download_parcel_universe_data.sh Makefile | ../temp ../output
	bash $< $(PARCEL_YEAR) $(TRIAD_NAME)

.PHONY: download-current

include ../../shared/code/generic.make
