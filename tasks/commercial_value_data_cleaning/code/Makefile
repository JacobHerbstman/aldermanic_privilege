SHELL := bash

.PHONY: all link-inputs

all: ../output/multifamily_data_cleaned.csv

../output/multifamily_data_cleaned.csv: commercial_value_data_cleaning.R ../input/commercial_value_raw.csv | ../output
	$(R) $<

../input/commercial_value_raw.csv: ../../download_commercial_valuation_data/output/commercial_valuation_data.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/commercial_value_raw.csv

include ../../shell_functions.make
include ../../generic.make
