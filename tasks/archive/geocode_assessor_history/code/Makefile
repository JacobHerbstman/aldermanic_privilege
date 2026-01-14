SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files and directories for this task
# ─────────────────────────────────────────────────────────────────────────────

# The R script that will perform the work.
SCRIPT_FILE := link_assessor_history.R

# Define the two output files this task will create.
OUTPUT_SHP      := ../output/attom_historical_with_address.shp
OUTPUT_PARQUET  := ../output/attom_historical_missing_address.parquet
OUTPUT_FILES    := $(OUTPUT_SHP) $(OUTPUT_PARQUET)

# Define the two input files this task needs as symbolic links.
INPUT_HISTORICAL := ../input/chicago_attom_history.parquet
INPUT_CURRENT    := ../input/chicago_attom_2023.parquet

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

# THE FIX: .DEFAULT_GOAL can only have one target. We'll pick the main shapefile.
# The 'all' target still correctly depends on BOTH files.
.DEFAULT_GOAL := $(OUTPUT_SHP)
.PHONY: all
all: $(OUTPUT_FILES)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output files.
# This rule correctly lists multiple targets. If any are missing, the script will run.
# -----------------------------------------------------------------------------
$(OUTPUT_FILES): $(SCRIPT_FILE) $(INPUT_HISTORICAL) $(INPUT_CURRENT) | ../output
	@echo "→ [geocode_history] Running R script to link historical and current data..."
	$(R) $(SCRIPT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links for each dependency.
# This is cleaner than a phony target when inputs come from different sources.
# -----------------------------------------------------------------------------

# Rule to link the historical data from the 'process_attom_assessor_historical' task.
$(INPUT_HISTORICAL): ../../process_attom_assessor_historical/output/chicago_attom_history.parquet | ../input
	@echo "→ [geocode_history] Linking input: chicago_attom_history.parquet"
	ln -sf $< $@

# Rule to link the current data from the 'process_attom_assessor' task.
$(INPUT_CURRENT): ../../process_attom_assessor/output/chicago_attom_2023.parquet | ../input
	@echo "→ [geocode_history] Linking input: chicago_attom_2023.parquet"
	ln -sf $< $@

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules for creating directories, cleanup, etc.
# -----------------------------------------------------------------------------
include ../../generic.make