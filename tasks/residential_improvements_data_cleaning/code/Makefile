SHELL := bash
include ../../shell_functions.make 

.PHONY: all link-input
all: ../output/residential_cross_section.csv

# Convert CSV -> Parquet (directory) without loading into R memory
../output/residential_cross_section.csv: ../input/residential_improvement_characteristics.csv | ../output
	$(R) residential_improvements_cleaning.R "$<" "$@"

include ../../generic.make

# Symlink from ../../../data_raw/ -> ../input/
../input/residential_improvement_characteristics.csv: ../../../data_raw/Assessor_-_Single_and_Multi-Family_Improvement_Characteristics_20251111.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/residential_improvement_characteristics.csv