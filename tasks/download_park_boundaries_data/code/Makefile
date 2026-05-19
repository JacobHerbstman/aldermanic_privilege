SHELL := bash

all: ../output/cpd_park_boundaries.geojson ../output/cpd_park_boundaries_metadata.csv

../output/cpd_park_boundaries.geojson: download_park_boundaries_data.sh | ../output
	bash $<

../output/cpd_park_boundaries_metadata.csv: ../output/cpd_park_boundaries.geojson
	@test -s "$@"

include ../../shell_functions.make
include ../../generic.make
