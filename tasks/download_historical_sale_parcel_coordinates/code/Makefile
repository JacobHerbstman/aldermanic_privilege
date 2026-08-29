START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv

../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv: download_historical_sale_parcel_coordinates.R ../input/residential_sales_clean.parquet | ../output
	$(R) $< $(START_YEAR) $(END_YEAR)

../input/residential_sales_clean.parquet: ../../clean_residential_sales/output/residential_sales_clean.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/residential_sales_clean.parquet

include ../../shell_functions.make
include ../../generic.make
