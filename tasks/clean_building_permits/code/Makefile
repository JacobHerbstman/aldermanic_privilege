SHELL := bash

all: ../output/building_permits_clean.gpkg

../output/building_permits_clean.gpkg: building_permits_cleaning.R ../input/building_permits.csv | ../output
	$(R) building_permits_cleaning.R

../input/building_permits.csv: ../../download_building_permits/output/building_permits.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/building_permits.csv

include ../../shell_functions.make
include ../../generic.make
