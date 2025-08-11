SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# This is the REAL file this task will create.
OUTPUT_FILE_PERMITS := ../output/alderman_restrictiveness_scores_data.csv
OUTPUT_FILE_WARD := ../output/ward_monthly_panel_for_alderman_fe.csv

# The R script that does the work.
SCRIPT_FILE := create_restrictiveness_score_data.R

INPUT_WARD_PANEL := ../input/ward_panel.gpkg
INPUT_PERMITS := ../input/building_permits_clean.gpkg
INPUT_ALDERMEN := ../input/chicago_alderman_panel.csv
INPUT_CONTROLS := ../input/ward_controls.csv

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

all: $(OUTPUT_FILE_WARD) $(OUTPUT_FILE_PERMITS)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# ─────────────────────────────────────────────────────────────────────────────
$(OUTPUT_FILE_WARD) $(OUTPUT_FILE_PERMITS): $(SCRIPT_FILE) $(INPUT_WARD_PANEL) $(INPUT_PERMITS) $(INPUT_ALDERMEN) | ../output
	@echo "→ [create_alderman_score] Running R script to create alderman restrictiveness scores"
	$(R) $<

# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# ─────────────────────────────────────────────────────────────────────────────

$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@echo "→ [create_restrictiveness_score] Linking: $@ ← $<"
	ln -sf $< $@

$(INPUT_PERMITS): ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@echo "→ [create_restrictiveness_score] Linking: $@ ← $<"
	ln -sf $< $@

$(INPUT_ALDERMEN): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@echo "→ [create_restrictiveness_score] Linking: $@ ← $<"
	ln -sf $< $@

# Rule for linking demographic controls
$(INPUT_CONTROLS): ../../create_ward_controls/output/ward_controls.csv | ../input
	@echo "→ [create_restrictiveness_score] Linking: $@ ← $<"
	ln -sf $< $@

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: link-inputs
link-inputs: $(INPUT_WARD_PANEL) $(INPUT_PERMITS) $(INPUT_ALDERMEN) $(INPUT_CONTROLS)