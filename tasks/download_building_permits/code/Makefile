SHELL := bash

all: ../output/building_permits.csv ../output/building_permits_metadata.csv

../output/building_permits.csv: download_building_permits.sh | ../output
	bash $<

../output/building_permits_metadata.csv: ../output/building_permits.csv
	@test -s "$@"

include ../../shell_functions.make
include ../../generic.make
