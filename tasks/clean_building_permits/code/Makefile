SHELL := bash
include ../../shell_functions.make

all: ../output/building_permits_clean.gpkg

../output/building_permits_clean.gpkg: building_permits_cleaning.R ../input/Building_Permits_20251121.csv | ../output
	$(R) building_permits_cleaning.R

../input/Building_Permits_20251121.csv: ../../../data_raw/Building_Permits_20251121.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/Building_Permits_20251121.csv