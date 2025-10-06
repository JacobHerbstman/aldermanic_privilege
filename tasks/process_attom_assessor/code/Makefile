SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────
.DEFAULT_GOAL := ../output/chicago_attom_2023.gpkg
.PHONY: all
all: ../output/chicago_attom_2023.gpkg

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final file
# This depends on the R script and the phony 'setup-inputs' target, which
# guarantees the input symlinks exist before the script runs.
# -----------------------------------------------------------------------------
../output/chicago_attom_2023.gpkg: process_assessor.R | setup-inputs ../output
	@echo "→ [process_assessor] Running R script to process current assessor data..."
	$(R) process_assessor.R

# ─────────────────────────────────────────────────────────────────────────────
# Phony target to create all input symbolic links at once.
# This uses an absolute path via $(CURDIR) to create robust, unbreakable links.
# -----------------------------------------------------------------------------
.PHONY: setup-inputs
setup-inputs: | ../input
	@echo "→ [process_assessor] Linking all raw CSV inputs..."
	ln -sf ../../../data_raw/assessor/*.csv ../input

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules for cleanup and other common tasks
# -----------------------------------------------------------------------------
include ../../generic.make

