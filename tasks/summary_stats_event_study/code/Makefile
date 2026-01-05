SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Inputs
# ─────────────────────────────────────────────────────────────────────────────

INPUT_SALES  := ../input/sales_stacked_panel.csv
INPUT_RENTAL := ../input/rental_stacked_panel.csv

ALL_INPUTS := $(INPUT_SALES) $(INPUT_RENTAL)

SCRIPT := create_summary_stats.R

# ─────────────────────────────────────────────────────────────────────────────
# Outputs
# ─────────────────────────────────────────────────────────────────────────────

OUTPUT_TEX := ../output/summary_stats_event_study.tex
OUTPUT_CSV := ../output/summary_stats_event_study.csv

ALL_OUTPUTS := $(OUTPUT_TEX) $(OUTPUT_CSV)

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(ALL_OUTPUTS)

$(ALL_OUTPUTS): $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_SALES): ../../run_event_study_sales/input/sales_stacked_panel.csv | ../input
	ln -sf $< $@

$(INPUT_RENTAL): ../../run_event_study_rental/input/rental_stacked_panel.csv | ../input
	ln -sf $< $@

include ../../generic.make
