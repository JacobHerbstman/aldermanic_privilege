SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# This is the REAL file this task will create.
OUTPUT_FILE_1 := ../output/census_blocks_ward_switchers.gpkg

# The R script that does the work.
SCRIPT_FILE := merge_wards_blocks.R

# Define the INPUTS. These will all be created as symbolic links.
# Note: For shapefiles, we make the .shp file the "representative" target.


# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all
all: $(OUTPUT_FILE_1)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# This rule depends on the script and all three sets of input shapefiles.
# -----------------------------------------------------------------------------
$(OUTPUT_FILE_1) $(OUTPUT_FILE_2): $(SCRIPT_FILE) ../input/census_blocks_2010.csv ../input/ward_panel.gpkg | ../output
	@echo "→ [merge_wards_blocks] Running R script to merge shapefiles..."
	$(R) $(SCRIPT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# -----------------------------------------------------------------------------

# Rule for linking the raw 2010 Census Block CSV data.
# The recipe now links the specific CSV file from the data_raw directory.
../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	@echo "→ [merge_wards_blocks] Linking raw input: census_blocks_2010.csv"
	ln -sf ../../../data_raw/CensusBlockTIGER2010_20250721.csv $@

# Rule for linking the raw 2005 ward data from the 'data_raw' directory.
# This links the entire directory.
../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: ../input/census_blocks_2010.csv ../input/ward_panel.gpkg
