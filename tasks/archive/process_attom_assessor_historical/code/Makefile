SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files
# ─────────────────────────────────────────────────────────────────────────────
OUTPUT_FILE := ../output/chicago_attom_history.parquet
SCRIPT_FILE := process_assessor_historical.R
INPUT_DIR   := ../input/assessor_history

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────
.DEFAULT_GOAL := $(OUTPUT_FILE)
.PHONY: all
all: $(OUTPUT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final Parquet file
# -----------------------------------------------------------------------------
$(OUTPUT_FILE): $(SCRIPT_FILE) | setup-inputs ../output ../temp
	@echo "→ [process_historical] Running R script to create Parquet file..."
	$(R) $(SCRIPT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Phony target to create all input links at once.
# -----------------------------------------------------------------------------
.PHONY: setup-inputs
setup-inputs: | $(INPUT_DIR)
	@echo "→ [process_historical] Linking all raw CSV inputs..."
	# THE FIX: Use an absolute path for the source data.
	# '$(CURDIR)' is a special 'make' variable holding the absolute path
	# to the current directory (.../code/), guaranteeing a correct source path.
	ln -sf $(CURDIR)/../../../data_raw/assessor_history/*.csv $(INPUT_DIR)/

# ─────────────────────────────────────────────────────────────────────────────
# Rule to create the specific subdirectory needed for this task's inputs
# -----------------------------------------------------------------------------
$(INPUT_DIR):
	mkdir -p $@

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make