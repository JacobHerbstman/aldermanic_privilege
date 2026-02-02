SHELL := bash
include ../../shell_functions.make

all: ../output/permits_with_developers.csv

../output/permits_with_developers.csv: extract_developer_identities.R ../input/Building_Permits_20251121.csv | ../output
	$(R) extract_developer_identities.R

../input/Building_Permits_20251121.csv: ../../../data_raw/Building_Permits_20251121.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/Building_Permits_20251121.csv
