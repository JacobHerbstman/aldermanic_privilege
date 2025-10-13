SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# This is the REAL file this task will create in its own output folder.
OUTPUT_FILE := ../output/building_permits_clean.gpkg

# This is the SYMBOLIC LINK this task needs in its input folder.
INPUT_FILE := ../input/Building_Permits_20250109.csv

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := $(OUTPUT_FILE)
.PHONY: all
all: $(OUTPUT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# This rule's job is to run the R script. It depends on the script itself
# and on the INPUT_FILE symlink being present.
# ─────────────────────────────────────────────────────────────────────────────
$(OUTPUT_FILE): building_permits_cleaning.R $(INPUT_FILE) | ../output
	@echo "→ [clean_building_permits] Running R script to clean permits..."
	$(R) building_permits_cleaning.R

# ─────────────────────────────────────────────────────────────────────────────
# Rule to create the input symbolic link
# This rule's target is the symlink. Its prerequisite is the REAL raw data
# file. The recipe is the command to create the link.
# ─────────────────────────────────────────────────────────────────────────────
$(INPUT_FILE): ../../../data_raw/Building_Permits_20250109.csv | ../input
	@echo "→ [clean_building_permits] Linking raw data input..."
	ln -s $< $@

link-inputs: $(INPUT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules and includes
# The 'generic.make' file should handle creating the ../input and ../output
# directories, as well as the 'clean' target.
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make