SHELL := bash
include ../../shell_functions.make

all: ../output/land_transaction_sales_pin10_events.parquet ../output/land_transaction_sales_schema_check.csv ../output/land_transaction_sales_cleaning_diagnostics.csv

../output/land_transaction_sales_pin10_events.parquet: prepare_land_transaction_sales.R ../input/Assessor_-_Parcel_Sales_20251123.csv | ../output
	$(R) prepare_land_transaction_sales.R ../input/Assessor_-_Parcel_Sales_20251123.csv ../output/land_transaction_sales_pin10_events.parquet ../output/land_transaction_sales_schema_check.csv ../output/land_transaction_sales_cleaning_diagnostics.csv

../output/land_transaction_sales_schema_check.csv \
../output/land_transaction_sales_cleaning_diagnostics.csv: ../output/land_transaction_sales_pin10_events.parquet
	test -f $@

../input/Assessor_-_Parcel_Sales_20251123.csv: ../../../data_raw/Assessor_-_Parcel_Sales_20251123.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/Assessor_-_Parcel_Sales_20251123.csv
