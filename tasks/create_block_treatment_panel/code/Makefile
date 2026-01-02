SHELL := bash
include ../../shell_functions.make

SCRIPT := create_treatment_panel.R

# Inputs
INPUT_BLOCKS_2010 := ../input/census_blocks_2010.csv
INPUT_BLOCKS_2020 := ../input/census_blocks_2020.csv
INPUT_WARD_PANEL := ../input/ward_panel.gpkg
INPUT_ALD_PANEL := ../input/chicago_alderman_panel.csv
INPUT_STRICTNESS := ../input/alderman_restrictiveness_scores_month_FEs.csv

ALL_INPUTS := $(INPUT_BLOCKS_2010) $(INPUT_BLOCKS_2020) $(INPUT_WARD_PANEL) $(INPUT_ALD_PANEL) $(INPUT_STRICTNESS)

# Output
OUTPUT := ../output/block_treatment_panel.csv

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUT)

$(OUTPUT): $(SCRIPT) $(ALL_INPUTS) | ../output
	$(R) $(SCRIPT)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_BLOCKS_2010): ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

$(INPUT_BLOCKS_2020): ../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	ln -sf $< $@

$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

$(INPUT_ALD_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

$(INPUT_STRICTNESS): ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

include ../../generic.make
