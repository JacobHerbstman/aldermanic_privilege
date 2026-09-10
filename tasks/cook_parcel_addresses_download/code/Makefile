include ../../shared/code/shell_functions.make

all: ../report/parcel_addresses_2025_chicago.csv.log

../output/parcel_addresses_2025_chicago.csv: ../../../data_raw/construction_review/parcel_addresses_2025_chicago_20260710.csv source_snapshot.sha256 Makefile | ../output
	shasum -a 256 -c source_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

../report/parcel_addresses_2025_chicago.csv.log: ../../shared/code/report.py ../output/parcel_addresses_2025_chicago.csv | ../report
	$(PYTHON) $< ../output/parcel_addresses_2025_chicago.csv $@ pin

include ../../shared/code/generic.make
