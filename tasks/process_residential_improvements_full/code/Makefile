SHELL := bash
include ../../shell_functions.make

all: ../output/residential_improvements_panel.parquet

../output/residential_improvements_panel.parquet: process_improvements.R ../input/residential_improvement_characteristics_full.csv | ../output
	$(R) process_improvements.R

../input/residential_improvement_characteristics_full.csv: ../../archive/download_residential_improvements_full/output/residential_improvement_characteristics_full.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/residential_improvement_characteristics_full.csv

include ../../generic.make
