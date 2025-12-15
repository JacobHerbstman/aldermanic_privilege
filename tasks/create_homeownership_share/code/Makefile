SHELL := bash
include ../../shell_functions.make

# Output files
OUTPUT_WARD := ../output/ward_controls_2000_2023.csv
OUTPUT_BG := ../output/block_group_controls_2000_2023.csv

# Inputs
INPUT_WARD_PANEL := ../input/ward_panel.gpkg

.DEFAULT_GOAL := all

.PHONY: all
all: $(OUTPUT_WARD) $(OUTPUT_BG)

# Both outputs are created by a single R script run
# Use a sentinel file to avoid running twice
$(OUTPUT_WARD) $(OUTPUT_BG): create_homeownership_share.R $(INPUT_WARD_PANEL) | ../output
	$(R) $<

# Link the Ward Panel from the previous task
$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

link-inputs: $(INPUT_WARD_PANEL)

include ../../generic.make