include ../../shared/code/shell_functions.make

all: ../output/parcel_sales_city.csv ../report/parcel_sales_city.csv.log

../output/parcel_sales_city.csv: ../../../data_raw/replication_sources/parcel_sales_city_2006_2022_20260826.csv source_snapshot.sha256 Makefile | ../output
	shasum -a 256 -c source_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

../report/parcel_sales_city.csv.log: ../../shared/code/report.py ../output/parcel_sales_city.csv | ../report
	$(PYTHON) $< ../output/parcel_sales_city.csv $@ row_id

include ../../shared/code/generic.make
