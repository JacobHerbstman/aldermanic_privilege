include ../../shared/code/shell_functions.make

START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/residential_sales_clean.parquet

../output/residential_sales_clean.parquet: clean_residential_sales.R ../input/parcel_sales_city.csv Makefile ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $< $(START_YEAR) $(END_YEAR)

../input/parcel_sales_city.csv: ../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	ln -sf $< $@

link-inputs: ../input/parcel_sales_city.csv

include ../../shared/code/generic.make
