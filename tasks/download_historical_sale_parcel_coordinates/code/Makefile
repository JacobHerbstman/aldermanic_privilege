START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv

../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv: download_historical_sale_parcel_coordinates.R ../input/parcel_sales.csv | ../output
	Rscript $< $(START_YEAR) $(END_YEAR)

../input/parcel_sales.csv: ../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcel_sales.csv

include ../../generic.make
