SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# This is the REAL file this task will create.
OUTPUT_FILE := ../output/alderman_restrictiveness_score.csv

# The R script that does the work.
SCRIPT_FILE := create_restrictiveness_score.R

INPUT_WARD_PANEL := ../input/ward_monthly_panel_for_alderman_fe.csv

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

all: $(OUTPUT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output

# ─────────────────────────────────────────────────────────────────────────────
$(OUTPUT_FILE): $(SCRIPT_FILE) | ../output
	@echo "→ [create_strictness_score] Running R script to create strictness score"
	$(R) $(SCRIPT_FILE)


# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# -----------------------------------------------------------------------------

# Rule for linking ward monthly panel 
$(INPUT_WARD_PANEL): ../../data_for_alderman_strictness_scores/output/ward_monthly_panel_for_alderman_fe.csv | ../input
	@echo "→ [create_strictness_score] Linking: $@ ← $<"
	ln -sf $< $@

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# Your generic.make file will handle creating the ../output directory.
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(INPUT_WARD_PANEL) 