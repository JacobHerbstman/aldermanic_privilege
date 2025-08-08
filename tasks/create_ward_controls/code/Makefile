SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# This is the REAL file this task will create.
OUTPUT_FILE := ../output/ward_controls.csv

# The R script that does the work.
SCRIPT_FILE := create_ward_controls.R

INPUT_WARD_PANEL := ../input/ward_panel.gpkg

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := $(OUTPUT_FILE)
.PHONY: all
all: $(OUTPUT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# This rule only depends on the script since there are no inputs.
# ─────────────────────────────────────────────────────────────────────────────
$(OUTPUT_FILE): $(SCRIPT_FILE) $(INPUT_WARD_PANEL) | ../output
	@echo "→ [create_ward_controls] Running R script to create ward controls"
	$(R) $(SCRIPT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# ─────────────────────────────────────────────────────────────────────────────

$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@echo "→ [create_ward_controls] Linking: $@ ← $<"
	ln -sf $< $@

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: link-inputs
link-inputs: $(INPUT_WARD_PANEL)

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make