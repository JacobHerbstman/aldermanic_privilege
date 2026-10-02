include ../../shared/code/shell_functions.make

all: ../output/residential_sales_clean.parquet ../output/residential_sales_sample_flow.csv

../%/residential_sales_clean.parquet ../%/residential_sales_sample_flow.csv: clean_residential_sales.R \
	../input/parcel_sales_city.csv | ../output ../report
	$(R) $<

../input/parcel_sales_city.csv: ../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	ln -sf $< $@

link-inputs: ../input/parcel_sales_city.csv

include ../../shared/code/generic.make
