all: ../output/parcel_addresses_2025_chicago.csv

download-current: ../output/parcel_addresses_2025_chicago_current.csv

../output/parcel_addresses_2025_chicago.csv: ../../../data_raw/construction_review/parcel_addresses_2025_chicago_20260710.csv source_snapshot.sha256 Makefile | ../output
	shasum -a 256 -c source_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

../output/parcel_addresses_2025_chicago_current.csv: Makefile | ../output
	wget -O $@.tmp 'https://datacatalog.cookcountyil.gov/resource/3723-97qp.csv?%24where=year%3D2025%20AND%20upper(prop_address_city_name)%3D%27CHICAGO%27&%24order=pin&%24limit=2000000'
	mv $@.tmp $@

.PHONY: download-current

include ../../shared/code/generic.make
