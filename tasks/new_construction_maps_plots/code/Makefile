SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# --- Main Goal: Create the final map image ---
all: ../output/construction_map_2006_2014_w_density.pdf

# --- Rule to generate the map from the R script ---
../output/construction_map_2006_2014_w_density.pdf: new_construction_maps.R ../input/parcels_with_geometry.gpkg ../input/ward_panel.gpkg | ../output
	@echo "→ Generating construction map..."
	Rscript new_construction_maps.R

# --- Rules for linking input files from other tasks ---
../input/parcels_with_geometry.gpkg: ../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@ln -sf $< $@

# --- Helper to create all input links at once ---
link-inputs: ../input/parcels_with_geometry.gpkg ../input/ward_panel.gpkg

# --- Include generic rules for directory creation etc. ---
include ../../generic.make