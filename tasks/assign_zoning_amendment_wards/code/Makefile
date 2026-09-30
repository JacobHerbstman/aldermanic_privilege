include ../../shared/code/shell_functions.make

all: ../output/zoning_amendment_address_queries.csv ../output/census_geocoder_responses.csv \
	../output/zoning_amendment_wards.csv

../output/zoning_amendment_address_queries.csv: build_address_queries.R ../input/zoning_map_amendments.csv \
	| ../output ../report
	$(R) $<

../output/census_geocoder_responses.csv: ../sources/census_geocoder_responses_20260928.csv.gz \
	geocoder_snapshot.sha256 | ../output
	shasum -a 256 -c geocoder_snapshot.sha256
	gzip -dc $< > $@.tmp
	mv $@.tmp $@

refresh-geocodes: ../temp/census_geocoder_responses_current.csv

../temp/census_geocoder_responses_current.csv: geocode_amendment_addresses.R \
	../output/zoning_amendment_address_queries.csv | ../temp
	$(R) $<

../output/zoning_amendment_wards.csv: assign_amendment_wards.R ../output/zoning_amendment_address_queries.csv \
	../output/census_geocoder_responses.csv ../input/zoning_map_amendments.csv ../input/Wards_2014.geojson \
	../input/Wards_2015.geojson ../input/Wards_2024.geojson ../input/alderman_terms.csv \
	../input/street_centerlines_2013.zip | ../output ../report
	$(R) $<

../input/zoning_map_amendments.csv: ../../clean_zoning_map_amendments/output/zoning_map_amendments.csv | ../input
	ln -sf $< $@
../input/Wards_2014.geojson: ../../../data_raw/Wards_2014.geojson | ../input
	ln -sf $< $@
../input/Wards_2015.geojson: ../../../data_raw/Wards_2015.geojson | ../input
	ln -sf $< $@
../input/Wards_2024.geojson: ../../../data_raw/Wards_2024.geojson | ../input
	ln -sf $< $@
../input/alderman_terms.csv: ../../create_alderman_data/adjudication/alderman_terms.csv | ../input
	ln -sf $< $@
../input/street_centerlines_2013.zip: ../../download_chicago_gis_layers/output/street_centerlines_2013.zip | ../input
	ln -sf $< $@

link-inputs: ../input/zoning_map_amendments.csv ../input/Wards_2014.geojson ../input/Wards_2015.geojson \
	../input/Wards_2024.geojson ../input/alderman_terms.csv ../input/street_centerlines_2013.zip

include ../../shared/code/generic.make
