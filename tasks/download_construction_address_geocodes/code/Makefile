include ../../shared/code/shell_functions.make

GEOCODERS := census chicago

all: $(foreach provider,$(GEOCODERS),../output/address_geocodes_$(provider).csv)

../output/address_geocodes_%.csv: combine_address_geocodes.R ../../shared/code/save_data.R ../../shared/code/report.py ../input/address_geocodes_census_2026-09-07.csv ../input/address_geocodes_chicago_2026-09-07.csv ../input/address_geocodes_census_additional_2026-09-07.csv ../input/address_geocodes_chicago_additional_2026-09-07.csv source_snapshot.sha256 Makefile | ../output ../report
	shasum -a 256 -c source_snapshot.sha256
	$(R) $< $*

../input/address_geocodes_census_2026-09-07.csv: ../../../data_raw/construction_review/address_geocodes_census_2026-09-07.csv | ../input
	ln -sf $< $@

../input/address_geocodes_chicago_2026-09-07.csv: ../../../data_raw/construction_review/address_geocodes_chicago_2026-09-07.csv | ../input
	ln -sf $< $@

../input/address_geocodes_census_additional_2026-09-07.csv: ../../../data_raw/construction_review/address_geocodes_census_additional_2026-09-07.csv | ../input
	ln -sf $< $@

../input/address_geocodes_chicago_additional_2026-09-07.csv: ../../../data_raw/construction_review/address_geocodes_chicago_additional_2026-09-07.csv | ../input
	ln -sf $< $@

link-inputs: ../input/address_geocodes_census_2026-09-07.csv ../input/address_geocodes_chicago_2026-09-07.csv ../input/address_geocodes_census_additional_2026-09-07.csv ../input/address_geocodes_chicago_additional_2026-09-07.csv

include ../../shared/code/generic.make
