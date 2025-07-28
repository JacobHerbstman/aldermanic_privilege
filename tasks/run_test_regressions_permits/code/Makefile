SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# This is the REAL file this task will create.
OUTPUT_FILE := ../output/results.txt

# The R script that does the work.
SCRIPT_FILE := test_regressions_permits.R

INPUT_UNBALANCED_PANEL := ../input/permit_regression_panel_blocks_unbalanced.csv"
INPUT_BALANCED_PANEL := ../input/permit_regression_panel_blocks_balanced.csv"

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

## rule for linking regression panels
$(INPUT_UNBALANCED_PANEL): ../../merge_permits_blocks/output/permit_regression_panel_blocks_unbalanced.csv | ../input
	@echo "→ [merge_parcels_blocks] Linking unbalanced regression panel"
	ln -sf ../../merge_permits_blocks/output/permit_regression_panel_blocks_unbalanced.csv ../input/

## rule for linking regression panels
$(INPUT_BALANCED_PANEL): ../../merge_permits_blocks/output/permit_regression_panel_blocks_balanced.csv | ../input
	@echo "→ [merge_parcels_blocks] Linking unbalanced regression panel"
	ln -sf ../../merge_permits_blocks/output/permit_regression_panel_blocks_balanced.csv ../input/

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# Your generic.make file will handle creating the ../output directory.
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(INPUT_UNBALANCED_PANEL)  $(INPUT_BALANCED_PANEL)