SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# Define all necessary input files for the R script
ALL_INPUTS := \
	../input/land_values_aug.parquet 

# The main target for this task is the final CSV output
#all: ../output/land_values_rd_data.csv

# Build rule: The output depends on the script and all its inputs.
../output/land_values_rd_data.csv: land_values_rd_data.R $(ALL_INPUTS) | ../output
	$(R) $<

# -------------------- Symlinks to bring in data from other tasks --------------------
../input/land_values_aug.parquet ../output/episode_metadata.csv: ../../calculate_distances_land_values/output/land_values_aug.parquet | ../input
	ln -sf "$<" "$@"

# Convenience target to create all symlinks
link-inputs: $(ALL_INPUTS)

# Include generic rules for creating directories and cleaning
include ../../generic.make

