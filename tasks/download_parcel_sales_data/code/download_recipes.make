include ../../shared/code/shell_functions.make

START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/parcel_sales_city_current.csv

../output/parcel_sales_city_current.csv: download_parcel_sales_data.sh download_recipes.make | ../output
	bash $< $(START_YEAR) $(END_YEAR)

include ../../shared/code/generic.make
