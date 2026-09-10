include ../../shared/code/shell_functions.make

all: ../output/parcel_universe_2025_city.csv ../report/parcel_universe_2025_city.csv.log ../report/parcel_universe_2025_city_native.csv.log

../output/parcel_universe_2025_city.csv: rewrite_parcel_universe_schema.R ../temp/parcel_universe_2025_city_native.csv | ../output
	$(R) $<

../report/parcel_universe_2025_city.csv.log: ../../shared/code/report.py ../output/parcel_universe_2025_city.csv | ../report
	$(PYTHON) $< ../output/parcel_universe_2025_city.csv $@ pin

../report/parcel_universe_2025_city_native.csv.log: ../../shared/code/report.py ../temp/parcel_universe_2025_city_native.csv | ../report
	$(PYTHON) $< ../temp/parcel_universe_2025_city_native.csv $@ pin

../temp/parcel_universe_2025_city_native.csv: ../../../data_raw/construction_review/parcel_universe_2025_city_native.csv parcel_universe_snapshot.sha256 Makefile | ../temp
	shasum -a 256 -c parcel_universe_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

include ../../shared/code/generic.make
