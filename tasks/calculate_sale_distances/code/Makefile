SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Task Configuration
# ─────────────────────────────────────────────────────────────────────────────

# Inputs (Symlinks)
INPUT_SALES        := ../input/Assessor_-_Parcel_Sales_20251123.csv
INPUT_PARCELS      := ../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv
INPUT_WARD_PANEL   := ../input/ward_panel.gpkg
INPUT_ALD_PANEL    := ../input/chicago_alderman_panel.csv
INPUT_STRICTNESS   := ../input/alderman_restrictiveness_scores_month_FEs.csv
INPUT_CONTROLS     := ../input/ward_controls.csv

ALL_INPUTS := $(INPUT_SALES) $(INPUT_PARCELS) $(INPUT_WARD_PANEL) \
              $(INPUT_ALD_PANEL) $(INPUT_STRICTNESS) $(INPUT_CONTROLS)

# ─────────────────────────────────────────────────────────────────────────────
# Options
# ─────────────────────────────────────────────────────────────────────────────

# Set to TRUE to run on a 5% sample for testing, FALSE for full run
SAMPLE := FALSE

# Determine output filename based on SAMPLE flag
ifeq ($(SAMPLE),TRUE)
  OUTPUT_FILE := ../output/sales_with_ward_distances_sample.csv
else
  OUTPUT_FILE := ../output/sales_with_ward_distances.csv
endif

# Script
SCRIPT_FILE := calculate_sale_distances.R

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
	@echo "→ [calculate_sale_distances] Running spatial join (Sample=$(SAMPLE))..."
	$(R) $(SCRIPT_FILE) --sample=$(SAMPLE)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_SALES): ../../../data_raw/Assessor_-_Parcel_Sales_20251123.csv | ../input
	@echo "→ [calculate_sale_distances] Linking sales data..."
	ln -sf $< $@

$(INPUT_PARCELS): ../../../data_raw/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv | ../input
	@echo "→ [calculate_sale_distances] Linking parcel universe..."
	ln -sf $< $@

$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@echo "→ [calculate_sale_distances] Linking ward panel..."
	ln -sf $< $@

$(INPUT_ALD_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@echo "→ [calculate_sale_distances] Linking alderman panel..."
	ln -sf $< $@

$(INPUT_STRICTNESS): ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	@echo "→ [calculate_sale_distances] Linking strictness scores..."
	ln -sf $< $@

$(INPUT_CONTROLS): ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	@echo "→ [calculate_sale_distances] Linking ward controls..."
	ln -sf $< $@

include ../../generic.make
