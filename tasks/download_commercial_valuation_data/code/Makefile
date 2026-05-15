SHELL := bash

all: ../output/commercial_valuation_data.csv

../output/commercial_valuation_data.csv: download_commercial_valuation_data.sh | ../output
	bash $<

include ../../shell_functions.make
include ../../generic.make
