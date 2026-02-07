SHELL := bash
include ../../shell_functions.make

all: ../output/building_permit_parcel_features.csv.gz

../output/building_permit_parcel_features.csv.gz: building_permits_scraping.R ../input/Building_Permits_20251121.csv | ../output
	$(R) building_permits_scraping.R

../input/Building_Permits_20251121.csv: ../../../data_raw/Building_Permits_20251121.csv | ../input
	ln -sf $< $@

link-inputs: ../input/Building_Permits_20251121.csv

include ../../generic.make
