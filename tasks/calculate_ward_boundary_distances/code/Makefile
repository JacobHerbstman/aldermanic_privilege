SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# --- Main Goal ---
all: ../output/parcels_with_ward_distances.gpkg

# --- Main processing rule ---
../output/parcels_with_ward_distances.gpkg: calculate_distances.R ../input/year_built_sample.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/alderman_strictness_scores.csv | ../output
	Rscript calculate_distances.R

# --- Rules that make each symlink depend on its source file ---
../input/year_built_sample.gpkg: ../../process_attom_assessor/output/chicago_attom_2023.gpkg | ../input
	@ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@ln -sf $< $@

../input/alderman_strictness_scores.csv: ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	@ln -sf $< $@

# --- The link-inputs target is a helper to create all links at once ---
link-inputs: ../input/year_built_sample.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/alderman_strictness_scores.csv

include ../../generic.make