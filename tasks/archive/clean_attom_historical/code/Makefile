# Makefile for clean_attom_historical task
# Takes geocoded assessor history and ATTOM data to produce year_built_sample.shp

SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

# Input sources (from other tasks)
GEOCODED_SOURCE := ../../geocode_assessor_history/output/attom_historical_with_address
ATTOM_SOURCE := ../../process_attom_assessor_historical/output/chicago_attom_history.parquet

# Local directories
INPUT_DIR := ../input
OUTPUT_DIR := ../output

# Symbolic links
GEOCODED_INPUT := $(INPUT_DIR)/attom_historical_with_address.shp
ATTOM_INPUT := $(INPUT_DIR)/chicago_attom_history.parquet

# Processing script and final output
SCRIPT := clean_address_sample.R
OUTPUT := $(OUTPUT_DIR)/year_built_sample.gpkg

# ─────────────────────────────────────────────────────────────────────────────
# Main Target
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUT)

# ─────────────────────────────────────────────────────────────────────────────
# Build Rules
# ─────────────────────────────────────────────────────────────────────────────

$(OUTPUT): $(SCRIPT) $(GEOCODED_INPUT) $(ATTOM_INPUT) | $(OUTPUT_DIR)
	@echo "→ Processing data to create year_built_sample.shp..."
	Rscript $(SCRIPT)

# ─────────────────────────────────────────────────────────────────────────────
# Input Dependencies
# ─────────────────────────────────────────────────────────────────────────────

$(GEOCODED_INPUT): | $(INPUT_DIR)
	@echo "→ Linking geocoded assessor data..."
	ln -sf $(GEOCODED_SOURCE).* $(INPUT_DIR)/

$(ATTOM_INPUT): | $(INPUT_DIR)
	@echo "→ Linking ATTOM historical data..."
	ln -sf $(ATTOM_SOURCE) $(INPUT_DIR)/

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(GEOCODED_INPUT) $(ATTOM_INPUT)
