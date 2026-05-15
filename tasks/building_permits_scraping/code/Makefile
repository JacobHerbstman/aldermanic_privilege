SHELL := bash

all: ../output/building_permit_parcel_features.csv.gz ../output/building_permit_project_features.csv.gz ../output/building_permits_text_features.csv.gz ../output/building_permits_extraction_sample.csv

../output/building_permit_parcel_features.csv.gz: building_permits_scraping.R ../input/building_permits.csv | ../output
	$(R) building_permits_scraping.R

../output/building_permit_project_features.csv.gz ../output/building_permits_text_features.csv.gz ../output/building_permits_extraction_sample.csv: ../output/building_permit_parcel_features.csv.gz
	@test -s "$@"
../input/building_permits.csv: ../../download_building_permits/output/building_permits.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/building_permits.csv

include ../../shell_functions.make
include ../../generic.make
