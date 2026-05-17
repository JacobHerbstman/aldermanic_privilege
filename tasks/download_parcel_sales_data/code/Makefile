SHELL := bash

all: ../output/parcel_sales_city.csv ../output/parcel_sales_city_metadata.csv

../output/parcel_sales_city.csv: download_parcel_sales_data.sh | ../output
	bash $<

../output/parcel_sales_city_metadata.csv: ../output/parcel_sales_city.csv
	@test -s "$@"

include ../../shell_functions.make
include ../../generic.make
