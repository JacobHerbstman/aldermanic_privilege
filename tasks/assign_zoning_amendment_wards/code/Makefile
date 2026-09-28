include ../../shared/code/shell_functions.make

all: ../output/zoning_amendment_address_queries.csv ../output/census_geocoder_responses.csv \
	../output/zoning_amendment_wards.csv

../%/zoning_amendment_address_queries.csv ../%/census_geocoder_responses.csv: geocode_amendment_addresses.R \
	../input/zoning_map_amendments.csv | ../output ../report ../temp
	$(R) $<

../output/zoning_amendment_wards.csv: assign_amendment_wards.R ../output/zoning_amendment_address_queries.csv \
	../output/census_geocoder_responses.csv ../input/zoning_map_amendments.csv ../input/Wards_2014.geojson \
	../input/Wards_2015.geojson ../input/Wards_2024.geojson ../input/alderman_terms.csv | ../output ../report
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

link-inputs: ../input/zoning_map_amendments.csv ../input/Wards_2014.geojson ../input/Wards_2015.geojson \
	../input/Wards_2024.geojson ../input/alderman_terms.csv

include ../../shared/code/generic.make
