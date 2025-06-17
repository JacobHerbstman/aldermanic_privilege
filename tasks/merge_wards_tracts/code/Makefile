SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# This is the REAL file this task will create.
OUTPUT_FILE := ../output/census_tract_panel_wards.shp

# The R script that does the work.
SCRIPT_FILE := merge_wards_tracts.R

# Define the INPUTS. These will all be created as symbolic links.
# Note: For shapefiles, we make the .shp file the "representative" target.
INPUT_TRACTS     := ../input/census_tracts.shp
INPUT_WARD_2005  := ../input/CHI_2005/CHI_2005.shp
INPUT_WARD_2015  := ../input/CHI_2015/CHI_2015.shp

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := $(OUTPUT_FILE)
.PHONY: all
all: $(OUTPUT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# This rule depends on the script and all three sets of input shapefiles.
# -----------------------------------------------------------------------------
$(OUTPUT_FILE): $(SCRIPT_FILE) $(INPUT_TRACTS) $(INPUT_WARD_2005) $(INPUT_WARD_2015) | ../output
	@echo "→ [merge_wards_tracts] Running R script to merge shapefiles..."
	$(R) $(SCRIPT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# -----------------------------------------------------------------------------

# Rule for linking the intermediate data from the 'clean_tract_data' task.
# The recipe uses a wildcard (*) to link all shapefile components (.shp, .shx, .dbf, etc).
$(INPUT_TRACTS): ../../clean_tract_data/output/census_tracts.shp | ../input
	@echo "→ [merge_wards_tracts] Linking intermediate input: census_tracts"
	ln -s ../../clean_tract_data/output/census_tracts.* ../input/

# Rule for linking the raw 2005 ward data from the 'data_raw' directory.
# This links the entire directory.
$(INPUT_WARD_2005): ../../../data_raw/CHI_2005 | ../input
	@echo "→ [merge_wards_tracts] Linking raw input: CHI_2005"
	mkdir -p $(dir $@) # Ensure the parent CHI_2005 directory exists in input
	ln -s ../../../data_raw/CHI_2005/* $(dir $@)


# Rule for linking the raw 2015 ward data.
$(INPUT_WARD_2015): ../../../data_raw/CHI_2015 | ../input
	@echo "→ [merge_wards_tracts] Linking raw input: CHI_2015"
	mkdir -p $(dir $@) # Ensure the parent CHI_2015 directory exists in input
	ln -s ../../../data_raw/CHI_2015/* $(dir $@)


# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make