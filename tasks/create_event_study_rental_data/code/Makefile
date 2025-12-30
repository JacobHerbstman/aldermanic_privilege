SHELL := bash
include ../../shell_functions.make

SCRIPT := create_event_study_data.R

# Inputs
INPUT_RENTALS    := ../input/rent_with_ward_distances.parquet
INPUT_WARD_PANEL := ../input/ward_panel.gpkg
INPUT_BLOCKS     := ../input/census_blocks_2010.csv
INPUT_ALD_PANEL  := ../input/chicago_alderman_panel.csv
INPUT_STRICTNESS := ../input/alderman_restrictiveness_scores_month_FEs.csv

ALL_INPUTS := $(INPUT_RENTALS) $(INPUT_WARD_PANEL) $(INPUT_BLOCKS) $(INPUT_ALD_PANEL) $(INPUT_STRICTNESS)

# Outputs
OUTPUT_YEARLY    := ../output/rental_stacked_panel.csv
OUTPUT_QUARTERLY := ../output/rental_stacked_quarterly_panel.csv

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUT_YEARLY) $(OUTPUT_QUARTERLY)

$(OUTPUT_YEARLY) $(OUTPUT_QUARTERLY): $(SCRIPT) $(ALL_INPUTS) | ../output
	$(R) $(SCRIPT)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_RENTALS): ../../calculate_rent_distances/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf $< $@

$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

$(INPUT_BLOCKS): ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

$(INPUT_ALD_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

$(INPUT_STRICTNESS): ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

include ../../generic.make
