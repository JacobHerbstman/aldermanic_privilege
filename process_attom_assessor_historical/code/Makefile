SHELL := bash

# pull in generic rules (exports PROJECT_ROOT, makes ../input ../temp ../output, etc)
include ../../shell_functions.make
include ../../generic.make

# Default target
all: ../output/chicago_attom_history.parquet

# Gather all the raw CSVs under ../input/assessor_history
INPUT_CSV := $(wildcard ../input/assessor_history/*.csv)

# Build the Parquet from those CSVs (and your R script)
../output/chicago_attom_history.parquet: \
    process_assessor_historical.R \
    $(INPUT_CSV) | load-packages ../temp ../output
	Rscript process_assessor_historical.R




