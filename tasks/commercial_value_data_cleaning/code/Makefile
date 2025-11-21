SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

all: ../output/multifamily_data_cleaned.csv

../output/multifamily_data_cleaned.csv: commercial_value_data_cleaning.R ../input/commercial_value_raw.csv | ../output
	$(R) $< $@

# Symlink
../input/commercial_value_raw.csv: ../../../data_raw/Assessor_-_Commercial_Valuation_Data_20251120.csv | ../input
	ln -sf "$<" "$@"

# Convenience
link-inputs: ../input/commercial_value_raw.csv

include ../../generic.make