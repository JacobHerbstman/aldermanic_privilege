SHELL := bash
include ../../shell_functions.make

# Output file
OUTPUT_FILE := ../output/ward_year_panel.csv

# Inputs
INPUT_WARD_PANEL := ../input/ward_panel.gpkg

.DEFAULT_GOAL := $(OUTPUT_FILE)

# Run the R script
$(OUTPUT_FILE): create_homeownership_share.R $(INPUT_WARD_PANEL) | ../output
	$(R) $<

# Link the Ward Panel from the previous task
$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

link-inputs: $(INPUT_WARD_PANEL)

include ../../generic.make