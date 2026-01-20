SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

SCRIPT := analyze_within_ward_variation.R

STRICTNESS_SCORES := ../input/alderman_restrictiveness_scores_month_FEs.csv
ALDERMAN_PANEL := ../input/chicago_alderman_panel.csv

ALL_INPUTS := $(STRICTNESS_SCORES) $(ALDERMAN_PANEL)

MAIN_OUTPUT := ../output/within_ward_dotplot.pdf

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs clean

all: $(MAIN_OUTPUT)

$(MAIN_OUTPUT): $(SCRIPT) $(ALL_INPUTS)
	Rscript $(SCRIPT)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

$(STRICTNESS_SCORES): ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

$(ALDERMAN_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

link-inputs: $(ALL_INPUTS)

include ../../generic.make
