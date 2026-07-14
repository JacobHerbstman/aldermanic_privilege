START_YEAR ?= 1999
END_YEAR ?= 2025

all: ../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv

../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv: download_historical_sale_parcel_coordinates.R ../input/parcel_sales.csv ../input/parcel_universe_2025_city.csv | ../output
	Rscript $< $(START_YEAR) $(END_YEAR)

../input/parcel_sales.csv: ../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcel_sales.csv ../input/parcel_universe_2025_city.csv

include ../../generic.make
