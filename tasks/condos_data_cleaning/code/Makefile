SHELL := bash
include ../../shell_functions.make 

.PHONY: all link-input
all: ../output/condos_cross_section.csv

# Convert CSV -> Parquet (directory) without loading into R memory
../output/condos_cross_section.csv: ../input/condos_characteristics.csv | ../output
	$(R) condos_cleaning.R "$<" "$@"

include ../../generic.make

# Symlink from ../../../data_raw/ -> ../input/
../input/condos_characteristics.csv: ../../../data_raw/Assessor_-_Residential_Condominium_Unit_Characteristics_20251002.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/condos_characteristics.csv