SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# This is the REAL file this task will create.
OUTPUT_FILE := ../output/alderman_restrictiveness_score_test.csv

# The R script that does the work.
SCRIPT_FILE := create_test_restrictiveness_score.R

INPUT_WARD_PANEL := ../input/ward_panel.gpkg

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := $(OUTPUT_FILE)
.PHONY: all
all: $(OUTPUT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output

# ─────────────────────────────────────────────────────────────────────────────
$(OUTPUT_FILE): $(SCRIPT_FILE) | ../output
	@echo "→ [create_alderman_score_test] Running R script to create_alderman_score_test"
	$(R) $(SCRIPT_FILE)


# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# -----------------------------------------------------------------------------

# Rule for linking the raw 2010 Census Block CSV data.
# The recipe now links the specific CSV file from the data_raw directory.
$(INPUT_WARD_PANEL): ../../merge_wards_blocks/output/ward_panel.gpkg | ../input
	@echo "→ [merge_parcels_blocks] Linking intermediate input: ward_panel"
	ln -sf ../../merge_wards_blocks/output/ward_panel.* ../input/

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# Your generic.make file will handle creating the ../output directory.
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(INPUT_WARD_PANEL) 