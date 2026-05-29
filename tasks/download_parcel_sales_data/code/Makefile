all: ../output/parcel_sales_city.csv

../output/parcel_sales_city.csv: download_parcel_sales_data.sh | ../output
	bash $<

include ../../generic.make
