SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# --- Main Goal: Create the final map image ---
all: ../output/zoning_district_map.pdf

# --- Rule to generate the map from the R script ---
../output/zoning_district_map.pdf: zoning_district_maps ../input/zoning_data_clean.gpkg| ../output
	@echo "→ Generating construction map..."
	Rscript new_construction_maps.R

# --- Rules for linking input files from other tasks ---
../input/zoning_data_clean.gpkg: ../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
	@ln -sf $< $@

# --- Helper to create all input links at once ---
link-inputs: ../input/zoning_data_clean.gpkg

# --- Include generic rules for directory creation etc. ---
include ../../generic.make