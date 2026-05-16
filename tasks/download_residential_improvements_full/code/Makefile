SHELL := bash

all: ../output/residential_improvement_characteristics_full.csv ../output/residential_improvement_characteristics_full_metadata.csv

../output/residential_improvement_characteristics_full.csv: download_residential_improvements.sh | ../output
	bash $<

../output/residential_improvement_characteristics_full_metadata.csv: ../output/residential_improvement_characteristics_full.csv
	@test -s "$@"

include ../../shell_functions.make
include ../../generic.make
