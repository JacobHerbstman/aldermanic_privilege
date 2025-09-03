SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# This is the REAL file this task will create.
OUTPUT_UNBALANCED := ../output/permit_regression_panel_blocks_unbalanced.csv
OUTPUT_BALANCED := ../output/permit_regression_panel_blocks_balanced.csv


# The R script that does the work.
SCRIPT_FILE := merge_permits_blocks.R

# Define the INPUTS. These will all be created as symbolic links.
# Note: For shapefiles, we make the .shp file the "representative" target.
INPUT_BLOCKS    := ../input/census_blocks_2010.csv
INPUT_PERMITS   := ../input/building_permits_clean.gpkg
INPUT_WARD_PANEL := ../input/ward_panel.gpkg
INPUT_BLOCK_PANEL := ../input/census_blocks_ward_switchers.gpkg
INPUT_RESTRICTIVENESS_SCORES := ../input/alderman_restrictiveness_score.csv
INPUT_CONTROLS := ../input/block_group_controls.csv
INPUT_ALDERMAN_PANEL := ../input/alderman_panel.csv

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all
all: $(OUTPUT_UNBALANCED) $(OUTPUT_BALANCED)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# This rule depends on the script and all three sets of input shapefiles.
# -----------------------------------------------------------------------------
$(OUTPUT_UNBALANCED) $(OUTPUT_BALANCED): $(SCRIPT_FILE) $(INPUT_BLOCKS) $(INPUT_PERMITS) $(INPUT_WARD_PANEL) $(INPUT_BLOCK_PANEL) $(INPUT_RESTRICTIVENESS_SCORES) $(INPUT_CONTROLS) $(INPUT_ALDERMAN_PANEL) | ../output
	@echo "→ [merge_wards_blocks] Running R script to merge parcels with census blocks"
	$(R) $(SCRIPT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# -----------------------------------------------------------------------------

# Rule for linking the raw 2010 Census Block CSV data.
# The recipe now links the specific CSV file from the data_raw directory.
$(INPUT_BLOCKS): ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	@echo "→ [merge_parcels_blocks] Linking raw input: census_blocks_2010.csv"
	ln -sf $< $@

# Rule for linking the intermediate parcel data from the 'clean_attom_historical' task.
$(INPUT_PERMITS): ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@echo "→ [merge_parcels_blocks] Linking intermediate input: year_built_sample"
	ln -sf $< $@

# Rule for linking the intermediate ward panel data.
# The recipe uses a wildcard (*) to link all shapefile components.
$(INPUT_WARD_PANEL): ../../merge_wards_blocks/output/ward_panel.gpkg | ../input
	@echo "→ [merge_parcels_blocks] Linking intermediate input: ward_panel"
	ln -sf $< $@

# Rule for linking the intermediate block panel data.
# The recipe uses a wildcard (*) to link all shapefile components.
$(INPUT_BLOCK_PANEL): ../../merge_wards_blocks/output/census_blocks_ward_switchers.gpkg | ../input
	@echo "→ [merge_parcels_blocks] Linking intermediate input: census_block_group_panel_wards"
	ln -sf $< $@

# Rule for linking restrictiveness scores
$(INPUT_RESTRICTIVENESS_SCORES): ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	@echo "→ [merge_parcels_blocks] Linking intermediate input: alderman restrictiveness scores"
	ln -sf $< $@

# Rule for linking demographic controls
$(INPUT_CONTROLS): ../../create_block_group_controls/output/block_group_controls.csv | ../input
	@echo "→ [merge_parcels_blocks] Linking intermediate input: census_block_group_panel_wards"
	ln -sf $< $@

$(INPUT_ALDERMAN_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@echo "→ [merge_parcels_blocks] Linking intermediate input: alderman panel"
	ln -sf $< $@
# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(INPUT_BLOCKS) $(INPUT_PERMITS) $(INPUT_WARD_PANEL) $(INPUT_BLOCK_PANEL) $(INPUT_RESTRICTIVENESS_SCORES) $(INPUT_CONTROLS) $(INPUT_ALDERMAN_PANEL)

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make


