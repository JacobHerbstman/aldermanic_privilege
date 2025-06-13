SHELL := bash

# pull in generic rules (exports PROJECT_ROOT, makes ../input ../temp ../output, etc)
include ../../shell_functions.make

# Default target
.DEFAULT_GOAL := ../output/chicago_attom_history.parquet
.DELETE_ON_ERROR:

# Gather all the raw CSVs under ../input/assessor_history
INPUT_CSV := $(wildcard ../input/assessor_history/*.csv)

# Build the Parquet and then relocate the temp files
../output/chicago_attom_history.parquet: \
    process_assessor_historical.R \
    $(INPUT_CSV) | ../temp ../output
	Rscript process_assessor_historical.R

	@echo "→ Moving temp files to central storage…"
	mkdir -p ../../data_link/process_attom_assessor_historical_temp
	mv ../temp/* ../../data_link/process_attom_assessor_historical_temp/

	@echo "→ Re-linking ../temp → data_link/process_attom_assessor_historical_temp"
	rm -rf ../temp
	ln -s ../../data_link/process_attom_assessor_historical_temp ../temp

include ../../generic.make



