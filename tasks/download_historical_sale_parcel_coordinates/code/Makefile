include ../../shared/code/shell_functions.make

START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR).csv

download-current: ../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR)_current.csv

../output/historical_sale_parcel_coordinates_%.csv: ../sources/historical_sale_parcel_coordinates_%.csv.gz | ../output ../temp
	gzip -dc $< > ../temp/historical_sale_parcel_coordinates_$*.csv
	mv ../temp/historical_sale_parcel_coordinates_$*.csv $@

../output/historical_sale_parcel_coordinates_$(START_YEAR)_$(END_YEAR)_current.csv: download_historical_sale_parcel_coordinates.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile ../../setup_environment/code/packages.R ../input/residential_sales_clean.parquet | ../output ../report
	$(R) $< $(START_YEAR) $(END_YEAR)

../input/residential_sales_clean.parquet: ../../clean_residential_sales/output/residential_sales_clean.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/residential_sales_clean.parquet

include ../../shared/code/generic.make
