include ../../shared/code/shell_functions.make

START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/parcel_sales_city.csv

download-current: ../output/parcel_sales_city_current.csv

../output/parcel_sales_city.csv: ../../../data_raw/replication_sources/parcel_sales_city_2006_2022_20260826.csv source_snapshot.sha256 Makefile | ../output
	shasum -a 256 -c source_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

../output/parcel_sales_city_current.csv: download_parcel_sales_data.sh Makefile | ../output
	bash $< $(START_YEAR) $(END_YEAR)

.PHONY: download-current

include ../../shared/code/generic.make
