SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# This is the REAL file this task will create.
OUTPUT_FILE_1 := ../output/census_block_panel_wards.shp
OUTPUT_FILE_2 := ../output/ward_panel.shp

# The R script that does the work.
SCRIPT_FILE := merge_wards_blocks.R

# Define the INPUTS. These will all be created as symbolic links.
# Note: For shapefiles, we make the .shp file the "representative" target.
INPUT_BLOCKS    := ../input/census_blocks.shp
INPUT_WARD_2005  := ../input/CHI_2005/CHI_2005.shp
INPUT_WARD_2015  := ../input/CHI_2015/CHI_2015.shp

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all
all: $(OUTPUT_FILE_1) $(OUTPUT_FILE_2)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# This rule depends on the script and all three sets of input shapefiles.
# -----------------------------------------------------------------------------
$(OUTPUT_FILE_1) $(OUTPUT_FILE_2): $(SCRIPT_FILE) $(INPUT_BLOCKS) $(INPUT_WARD_2005) $(INPUT_WARD_2015) | ../output
	@echo "→ [merge_wards_blocks] Running R script to merge shapefiles..."
	$(R) $(SCRIPT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# -----------------------------------------------------------------------------

# Rule for linking the intermediate data from the 'clean_block_data' task.
# The recipe uses a wildcard (*) to link all shapefile components (.shp, .shx, .dbf, etc).
$(INPUT_BLOCKS): ../../clean_block_data/output/census_blocks.shp | ../input
	@echo "→ [merge_wards_blocks] Linking intermediate input: census_blocks"
	ln -sf ../../clean_block_data/output/census_blocks.* ../input/

# Rule for linking the raw 2005 ward data from the 'data_raw' directory.
# This links the entire directory.
$(INPUT_WARD_2005): ../../../data_raw/CHI_2005 | ../input
	@echo "→ [merge_wards_tracts] Linking raw input: CHI_2005"
	ln -sf ../../../data_raw/CHI_2005/* $(dir $@)

# Rule for linking the raw 2015 ward data.
$(INPUT_WARD_2015): ../../../data_raw/CHI_2015 | ../input
	@echo "→ [merge_wards_tracts] Linking raw input: CHI_2015"
	ln -sf ../../../data_raw/CHI_2015/* $(dir $@)

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(INPUT_BLOCKS) $(INPUT_WARD_2005) $(INPUT_WARD_2015)
