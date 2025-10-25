SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# Define all necessary input files for the R script
ALL_INPUTS := \
	../input/land_values_rd_data.csv \
	../input/episode_metadata.csv

# The main target for this task is the final CSV output
#all: ../output/rd_alderman_pair_episodes.csv

# Build rule: The output depends on the script and all its inputs.
../output/rd_alderman_pair_episodes.csv: land_values_rd.R $(ALL_INPUTS) | ../output
	$(R) land_values_rd.R

# -------------------- Symlinks to bring in data from other tasks --------------------
../input/land_values_rd_data.csv: ../../land_values_rd_data/output/land_values_rd_data.csv | ../input
	ln -sf "$<" "$@"

../input/episode_metadata.csv: ../../land_values_rd_data/output/episode_metadata.csv | ../input
	ln -sf "$<" "$@"

# Convenience target to create all symlinks
link-inputs: $(ALL_INPUTS)

# Include generic rules for creating directories and cleaning
include ../../generic.make

