SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

SCRIPT := analyze_within_ward_variation.R
UNCERTAINTY_SPEC := ptfeFALSE_rtfeFALSE_porchTRUE_cafeFALSE_2stage

STRICTNESS_SCORES := ../input/alderman_restrictiveness_scores_month_FEs.csv
UNCERTAINTY_SCORES := ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv
ALDERMAN_PANEL := ../input/chicago_alderman_panel.csv

ALL_INPUTS := $(STRICTNESS_SCORES) $(UNCERTAINTY_SCORES) $(ALDERMAN_PANEL)

MAIN_OUTPUT := ../output/within_ward_dotplot.pdf
UNCERTAINTY_MAIN_OUTPUT := ../output/within_ward_dotplot_uncertainty_$(UNCERTAINTY_SPEC).pdf

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs clean

all: $(MAIN_OUTPUT) $(UNCERTAINTY_MAIN_OUTPUT)

$(MAIN_OUTPUT): $(SCRIPT) $(STRICTNESS_SCORES) $(ALDERMAN_PANEL)
	Rscript $(SCRIPT)

$(UNCERTAINTY_MAIN_OUTPUT): $(SCRIPT) $(UNCERTAINTY_SCORES) $(ALDERMAN_PANEL)
	Rscript $(SCRIPT) $(UNCERTAINTY_SCORES) uncertainty_index "Regulatory Uncertainty Index" uncertainty_$(UNCERTAINTY_SPEC)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

$(STRICTNESS_SCORES): ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

$(UNCERTAINTY_SCORES): ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	ln -sf $< $@

$(ALDERMAN_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

link-inputs: $(ALL_INPUTS)

include ../../generic.make
