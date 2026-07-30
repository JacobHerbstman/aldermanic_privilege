START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/parcel_sales_city.csv

../output/parcel_sales_city.csv: download_parcel_sales_data.sh Makefile | ../output
	bash $< $(START_YEAR) $(END_YEAR)

include ../../generic.make
