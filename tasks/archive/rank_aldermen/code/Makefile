SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# Define all necessary input files for the R script
ALL_INPUTS := \
	../input/rd_alderman_pair_episodes.csv \
	../input/chicago_alderman_panel.csv

# The main target for this task is the final CSV output
#all: ../output/aldermen_strictness_scores.csv ../output/strictness_barplot_current_only.pdf ../output/strictness_barplot.pdf

# Build rule: The output depends on the script and all its inputs.
../output/aldermen_strictness_scores.csv strictness_barplot_current_only.pdf ../output/strictness_barplot.pdf: rank_aldermen.R $(ALL_INPUTS) | ../output
	$(R) $<

# -------------------- Symlinks to bring in data from other tasks --------------------
../input/rd_alderman_pair_episodes.csv: ../../land_values_rd/output/rd_alderman_pair_episodes.csv | ../input
	ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

# Convenience target to create all symlinks
link-inputs: $(ALL_INPUTS)

# Include generic rules for creating directories and cleaning
include ../../generic.make

