SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# This is the REAL file this task will create.
OUTPUT_FILE := ../output/census_tracts.shp

# The R script that does the work.
SCRIPT_FILE := census_tracts_tidy.R

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := $(OUTPUT_FILE)
.PHONY: all
all: $(OUTPUT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# This is a "first-stage" task, so it only depends on its own script.
# It does not need any linked inputs from other tasks.
# ─────────────────────────────────────────────────────────────────────────────
$(OUTPUT_FILE): $(SCRIPT_FILE) | ../output
	@echo "→ [clean_tract_data] Running R script to download census tracts..."
	$(R) $(SCRIPT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# Your generic.make file will handle creating the ../output directory.
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make