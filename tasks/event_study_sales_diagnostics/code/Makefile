SHELL := bash
include ../../shell_functions.make

SCRIPT := run_diagnostics.R

# Inputs
INPUT_STACKED    := ../input/sales_stacked_panel.csv
INPUT_STACKED_Q  := ../input/sales_stacked_quarterly_panel.csv
INPUT_BLOCKS     := ../input/census_blocks_2010.csv
INPUT_WARD_PANEL := ../input/ward_panel.gpkg
INPUT_TREATMENT  := ../input/block_treatment_panel.csv

ALL_INPUTS := $(INPUT_STACKED) $(INPUT_STACKED_Q) $(INPUT_BLOCKS) $(INPUT_WARD_PANEL) $(INPUT_TREATMENT)

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(ALL_INPUTS) | ../output
	$(R) $(SCRIPT)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_STACKED): ../../create_event_study_sales_data/output/sales_stacked_panel.csv | ../input
	ln -sf $< $@

$(INPUT_STACKED_Q): ../../create_event_study_sales_data/output/sales_stacked_quarterly_panel.csv | ../input
	ln -sf $< $@

$(INPUT_BLOCKS): ../../create_event_study_sales_data/input/census_blocks_2010.csv | ../input
	ln -sf $< $@

$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

$(INPUT_TREATMENT): ../../create_block_treatment_panel/output/block_treatment_panel.csv | ../input
	ln -sf $< $@

include ../../generic.make

