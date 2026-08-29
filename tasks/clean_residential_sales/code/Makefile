START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/residential_sales_clean.parquet

../output/residential_sales_clean.parquet: clean_residential_sales.R ../input/parcel_sales_city.csv Makefile | ../output
	$(R) $< $(START_YEAR) $(END_YEAR)

../input/parcel_sales_city.csv: ../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcel_sales_city.csv

include ../../shell_functions.make
include ../../generic.make
