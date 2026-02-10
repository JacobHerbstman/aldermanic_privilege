SHELL := bash
include ../../shell_functions.make

SCRIPT := analyze_within_ward_variation.R
UNCERTAINTY_SPEC := ptfeFALSE_rtfeFALSE_porchTRUE_cafeFALSE_2stage

UNCERTAINTY_SCORES := ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv
ALDERMAN_PANEL := ../input/chicago_alderman_panel.csv

TARGET := ../output/predecessor_successor_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(TARGET)

$(TARGET): $(SCRIPT) $(UNCERTAINTY_SCORES) $(ALDERMAN_PANEL) | ../output
	Rscript $(SCRIPT) $(UNCERTAINTY_SCORES) uncertainty_index "Regulatory Uncertainty Index" uncertainty_$(UNCERTAINTY_SPEC)
	rm -f ../output/ward_alderman_summary_uncertainty_$(UNCERTAINTY_SPEC).csv
	rm -f ../output/within_ward_dotplot_uncertainty_$(UNCERTAINTY_SPEC).pdf
	rm -f ../output/variance_decomposition_uncertainty_$(UNCERTAINTY_SPEC).csv

$(UNCERTAINTY_SCORES): ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	ln -sf "$<" "$@"

$(ALDERMAN_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

link-inputs: $(UNCERTAINTY_SCORES) $(ALDERMAN_PANEL)

include ../../generic.make
