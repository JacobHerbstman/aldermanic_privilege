include ../../shared/code/shell_functions.make

JOURNAL_YEARS := $(shell seq 2000 2011)

all: ../output/journal_amendment_places.csv

../output/journal_amendment_places.csv: place_journal_amendments.R \
	$(foreach year,$(JOURNAL_YEARS),../input/journal_introductions_$(year).csv) ../input/street_centerlines_2013.zip \
	../input/Chicago_Wards_1998.geojson ../input/Wards_2014.geojson ../input/alderman_terms.csv | ../output ../report
	$(R) $<

../input/journal_introductions_%.csv: ../../parse_journal_zoning_amendments/output/journal_introductions_%.csv \
	| ../input
	ln -sf $< $@
../input/street_centerlines_2013.zip: ../../download_chicago_gis_layers/output/street_centerlines_2013.zip | ../input
	ln -sf $< $@
../input/Chicago_Wards_1998.geojson: ../../../data_raw/Chicago_Wards_1998.geojson | ../input
	ln -sf $< $@
../input/Wards_2014.geojson: ../../../data_raw/Wards_2014.geojson | ../input
	ln -sf $< $@
../input/alderman_terms.csv: ../../create_alderman_data/adjudication/alderman_terms.csv | ../input
	ln -sf $< $@

link-inputs: $(foreach year,$(JOURNAL_YEARS),../input/journal_introductions_$(year).csv) \
	../input/street_centerlines_2013.zip ../input/Chicago_Wards_1998.geojson ../input/Wards_2014.geojson \
	../input/alderman_terms.csv

include ../../shared/code/generic.make
