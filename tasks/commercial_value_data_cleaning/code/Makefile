SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

all: ../output/multifamily_data_cleaned.csv

../output/multifamily_data_cleaned.csv: commercial_value_data_cleaning.R ../input/commercial_value_raw.csv ../input/approved_assessor_corrections.csv ../input/approved_commercial_density_blocker_corrections.csv | ../output
	$(R) commercial_value_data_cleaning.R ../input/commercial_value_raw.csv ../input/approved_assessor_corrections.csv ../input/approved_commercial_density_blocker_corrections.csv ../output/multifamily_data_cleaned.csv

../input/commercial_value_raw.csv: ../../../data_raw/Assessor_-_Commercial_Valuation_Data_20251120.csv | ../input
	ln -sf "$<" "$@"

../input/approved_assessor_corrections.csv: ../../new_construction_manual_corrections/manual/approved_assessor_corrections.csv | ../input
	ln -sf "$<" "$@"

../input/approved_commercial_density_blocker_corrections.csv: ../../commercial_density_blocker_manual_corrections/output/approved_commercial_density_blocker_corrections.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/commercial_value_raw.csv ../input/approved_assessor_corrections.csv ../input/approved_commercial_density_blocker_corrections.csv

include ../../generic.make
