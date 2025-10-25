SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# Define all necessary input files for the R script
ALL_INPUTS := \
	../input/land_values_aug.parquet \
	../input/ward_panel.gpkg \
	../input/census_blocks_ward_switchers.gpkg \

# The main target for this task is the final CSV output
all: ../output/land_event_study_data.csv

# Build rule: The output depends on the script and all its inputs.
../output/land_event_study_data.csv: create_land_event_study_data.R $(ALL_INPUTS) | ../output
	$(R) create_land_event_study_data.R

# -------------------- Symlinks to bring in data from other tasks --------------------
../input/land_values_aug.parquet: ../../calculate_distances_land_values/output/land_values_aug.parquet | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/census_blocks_ward_switchers.gpkg: ../../merge_wards_blocks/output/census_blocks_ward_switchers.gpkg | ../input
	@echo "→ [merge_wards_blocks] Linking raw input: census_blocks.csv"
	ln -sf ../../merge_wards_blocks/output/census_blocks_ward_switchers.gpkg  $@

# Convenience target to create all symlinks
link-inputs: $(ALL_INPUTS)

# Include generic rules for creating directories and cleaning
include ../../generic.make

