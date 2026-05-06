SHELL := bash
include ../../shell_functions.make

all: ../output/parcel_sales_city.csv

../output/parcel_sales_city.csv: download_parcel_sales_data.sh | ../output
	bash $< $@ ../output/parcel_sales_city_metadata.csv

include ../../generic.make
