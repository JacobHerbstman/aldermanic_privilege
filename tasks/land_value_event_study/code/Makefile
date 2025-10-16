SHELL := bash
include ../../shell_functions.make

.PHONY: all link-input

# Define all necessary input files for the R script
ALL_INPUTS := \
	../input/land_event_study_data.csv \

# The main target for this task is a "stamp" file. This file indicates that the
# R script, which generates many plot files in a loop, has completed successfully.
# This avoids having to list every single PDF as a dependency.
all: ../output/.plots_done

# Build rule: The stamp file depends on the R script and the input data.
# If either changes, the rule will re-run.
../output/.plots_done: land_value_event_study.R $(ALL_INPUTS) | ../output
	# Run the R script which contains the loop to generate all plots.
	$(R) land_value_event_study.R
	# Create the empty stamp file to signify completion.
	touch $@

# -------------------- Symlinks to bring in data from other tasks --------------------
../input/land_event_study_data.csv: ../../land_value_event_study_data/output/land_event_study_data.csv | ../input
	ln -sf "$<" "$@"

# Convenience target to create all symlinks
link-inputs: $(ALL_INPUTS)

# Include generic rules for creating directories and cleaning
include ../../generic.make
