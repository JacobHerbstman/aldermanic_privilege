SHELL := bash
include ../../shell_functions.make

SCRIPT := permit_summary_stats.R
INPUT := ../input/permits_for_uncertainty_index.csv

OUTPUTS := \
	../output/high_discretion_permit_counts.csv \
	../output/high_discretion_permit_counts.tex \
	../output/processing_time_raw_high_discretion.pdf \
	../output/processing_time_raw_new_construction.pdf \
	../output/processing_time_residualized_high_discretion.pdf \
	../output/processing_time_residualized_new_construction.pdf

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUTS)

$(OUTPUTS): $(SCRIPT) $(INPUT) | ../output
	Rscript $(SCRIPT) --input="$(INPUT)" --output_dir="../output"

$(INPUT): ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT)

include ../../generic.make
