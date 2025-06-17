SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files and directories for this task
# ─────────────────────────────────────────────────────────────────────────────

# The final, real output file this task will create.
OUTPUT_FILE := ../output/chicago_attom_2023.parquet

# The R script that will perform the work.
SCRIPT_FILE := process_assessor.R

# The directory where the symbolic links for the input CSVs will be created.
INPUT_DIR   := ../input/assessor

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := $(OUTPUT_FILE)
.PHONY: all
all: $(OUTPUT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final Parquet file.
# This depends on the R script and the phony 'setup-inputs' target, which
# guarantees the input symlinks exist before the script runs.
# -----------------------------------------------------------------------------
$(OUTPUT_FILE): $(SCRIPT_FILE) | setup-inputs ../output
	@echo "→ [process_assessor] Running R script to process current assessor data..."
	$(R) $(SCRIPT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Phony target to create all input symbolic links at once.
# This uses an absolute path via $(CURDIR) to create robust, unbreakable links.
# -----------------------------------------------------------------------------
.PHONY: setup-inputs
setup-inputs: | $(INPUT_DIR)
	@echo "→ [process_assessor] Linking all raw CSV inputs..."
	ln -sf $(CURDIR)/../../../data_raw/assessor/*.csv $(INPUT_DIR)/

# ─────────────────────────────────────────────────────────────────────────────
# Rule to create the specific subdirectory for this task's inputs
# -----------------------------------------------------------------------------
$(INPUT_DIR):
	mkdir -p $@

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules for cleanup and other common tasks
# -----------------------------------------------------------------------------
include ../../generic.make

