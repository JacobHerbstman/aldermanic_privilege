SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# --- Key Files ---
INPUT_PARCELS := ../input/parcels_with_ward_distances.gpkg
OUTPUT_FILE := ../output/output.txt
SCRIPT_FILE := spatial_rd.R

# --- Goals ---
all: $(OUTPUT_FILE)

# --- Main Rule ---
$(OUTPUT_FILE): $(INPUT_PARCELS) $(SCRIPT_FILE) | ../output
	@echo "→ [analyze_boundary_effects] Running R script to analyze boundary effects."
	Rscript $(SCRIPT_FILE)

# --- Input Linking ---
$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.gpkg | ../input
	@echo "→ [analyze_boundary_effects] Linking input: $@ ← $<"
	ln -sf $< $@

# --- Generic Rules & Utilities ---
include ../../generic.make
link-inputs: $(INPUT_PARCELS)
