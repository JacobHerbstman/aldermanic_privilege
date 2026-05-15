SHELL := bash

all: ../output/commercial_valuation_data.csv ../output/commercial_valuation_data_metadata.csv

../output/commercial_valuation_data.csv: download_commercial_valuation_data.sh | ../output
	bash $<

../output/commercial_valuation_data_metadata.csv: ../output/commercial_valuation_data.csv
	@test -s "$@"

include ../../shell_functions.make
include ../../generic.make
