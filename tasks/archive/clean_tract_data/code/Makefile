SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: all
all: ../output/census_tracts.gpkg

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# This is a "first-stage" task, so it only depends on its own script.
# ─────────────────────────────────────────────────────────────────────────────
../output/census_tracts.gpkg: clean_tract_data.R | ../output
	@echo "→ [clean_tract_data] Running R script to download census tracts..."
	Rscript $<

# ─────────────────────────────────────────────────────────────────────────────
# generic.make file will handle creating the ../output directory.
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make