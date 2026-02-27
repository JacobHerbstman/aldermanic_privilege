SHELL := bash
include ../../shell_functions.make

SCRIPT := analyze_within_ward_variation.R
UNCERTAINTY_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH

UNCERTAINTY_SCORES := ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv
ALDERMAN_PANEL := ../input/chicago_alderman_panel.csv

MAIN_OUTPUT := ../output/predecessor_successor_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf
AUX_OUTPUTS := \
	../output/ward_alderman_summary_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/within_ward_dotplot_uncertainty_$(UNCERTAINTY_SPEC).pdf \
	../output/variance_decomposition_uncertainty_$(UNCERTAINTY_SPEC).csv
ALL_OUTPUTS := $(MAIN_OUTPUT) $(AUX_OUTPUTS)

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(ALL_OUTPUTS)

$(MAIN_OUTPUT): $(SCRIPT) $(UNCERTAINTY_SCORES) $(ALDERMAN_PANEL) | ../output
	Rscript $(SCRIPT) $(UNCERTAINTY_SCORES) uncertainty_index "Regulatory Uncertainty Index" uncertainty_$(UNCERTAINTY_SPEC)

$(AUX_OUTPUTS): $(MAIN_OUTPUT)

$(UNCERTAINTY_SCORES): ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	ln -sf "$<" "$@"

$(ALDERMAN_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

link-inputs: $(UNCERTAINTY_SCORES) $(ALDERMAN_PANEL)

include ../../generic.make
