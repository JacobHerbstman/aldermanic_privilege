SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Task Configuration
# ─────────────────────────────────────────────────────────────────────────────

# Inputs (Symlinks)
INPUT_RENT_PANEL   := ../input/chicago_rent_panel.parquet
INPUT_WARD_PANEL   := ../input/ward_panel.gpkg
INPUT_ALD_PANEL    := ../input/chicago_alderman_panel.csv
INPUT_STRICTNESS   := ../input/alderman_restrictiveness_scores_month_FEs.csv
INPUT_CONTROLS     := ../input/ward_controls.csv

ALL_INPUTS := $(INPUT_RENT_PANEL) $(INPUT_WARD_PANEL) $(INPUT_ALD_PANEL) \
              $(INPUT_STRICTNESS) $(INPUT_CONTROLS)

# ─────────────────────────────────────────────────────────────────────────────
# Options
# ─────────────────────────────────────────────────────────────────────────────

# Set to TRUE to run on a 5% sample for testing, FALSE for full run
# You can override this from the command line: make SAMPLE=TRUE
SAMPLE := FALSE

# Determine output filename based on SAMPLE flag
ifeq ($(SAMPLE),TRUE)
  OUTPUT_FILE := ../output/rent_with_ward_distances_sample.parquet
else
  OUTPUT_FILE := ../output/rent_with_ward_distances_full.parquet
endif

# Script
SCRIPT_FILE := calculate_rent_distances.R

# ─────────────────────────────────────────────────────────────────────────────
# Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Main Rule
# ─────────────────────────────────────────────────────────────────────────────

$(OUTPUT_FILE): $(SCRIPT_FILE) $(ALL_INPUTS) | ../output
	@echo "→ [calculate_rent_distances] Running spatial join (Sample=$(SAMPLE))..."
	$(R) $(SCRIPT_FILE) --sample=$(SAMPLE)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_RENT_PANEL): ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	ln -sf $< $@

$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

$(INPUT_ALD_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

$(INPUT_STRICTNESS): ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

$(INPUT_CONTROLS): ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	ln -sf $< $@

include ../../generic.make