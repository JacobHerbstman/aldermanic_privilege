include ../../shared/code/shell_functions.make

START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/building_permits_clean.gpkg

../output/building_permits_clean.gpkg: building_permits_cleaning.R ../input/building_permits_$(START_YEAR)_$(END_YEAR).csv Makefile | ../output ../report
	$(R) $< $(START_YEAR) $(END_YEAR)

../input/building_permits_$(START_YEAR)_$(END_YEAR).csv: ../../download_building_permits/output/building_permits_$(START_YEAR)_$(END_YEAR).csv | ../input
	ln -sf $< $@

link-inputs: ../input/building_permits_$(START_YEAR)_$(END_YEAR).csv

include ../../shared/code/generic.make
