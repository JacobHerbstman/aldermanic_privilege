include ../../shared/code/shell_functions.make

START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv \
	../report/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv.log

../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv: download_historical_sale_parcel_coordinates.R Makefile ../../setup_environment/code/packages.R ../input/residential_sales_clean.parquet | ../output
	$(R) $< $(START_YEAR) $(END_YEAR)

../report/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv.log: ../../shared/code/report.py ../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv | ../report
	$(PYTHON) $< ../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv $@ pin year

../input/residential_sales_clean.parquet: ../../clean_residential_sales/output/residential_sales_clean.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/residential_sales_clean.parquet

include ../../shared/code/generic.make
