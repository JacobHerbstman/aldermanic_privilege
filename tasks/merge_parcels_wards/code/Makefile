# Makefile for merge_parcels_wards task
# merges geocoded assessor history and ward panel

SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

# Input sources (from other tasks)
PARCEL_SOURCE := ../../clean_attom_historical/output/year_built_sample
WARD_SOURCE := ../../merge_wards_tracts/output/ward_panel

# Local directories
INPUT_DIR := ../input
OUTPUT_DIR := ../output

# Symbolic links
PARCEL_INPUT := $(INPUT_DIR)/year_built_sample.shp
WARD_INPUT := $(INPUT_DIR)/ward_panel.shp

# Processing script and final output
SCRIPT := merge_parcels_wards.R
OUTPUT := $(OUTPUT_DIR)/parcel_panel.shp

# ─────────────────────────────────────────────────────────────────────────────
# Main Target
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUT)

# ─────────────────────────────────────────────────────────────────────────────
# Build Rules
# ─────────────────────────────────────────────────────────────────────────────

$(OUTPUT): $(SCRIPT) $(PARCEL_INPUT) $(WARD_INPUT) | $(OUTPUT_DIR)
	@echo "→ Processing data to create parcel_panel..."
	Rscript $(SCRIPT)

# ─────────────────────────────────────────────────────────────────────────────
# Input Dependencies
# ─────────────────────────────────────────────────────────────────────────────

$(PARCEL_INPUT): | $(INPUT_DIR)
	@echo "→ Linking geocoded assessor data..."
	ln -sf $(PARCEL_SOURCE).* $(INPUT_DIR)/

$(WARD_INPUT): | $(INPUT_DIR)
	@echo "→ Linking ATTOM historical data..."
	ln -sf $(WARD_SOURCE).* $(INPUT_DIR)/

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(PARCEL_INPUT) $(WARD_INPUT)
