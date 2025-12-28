SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Task Configuration
# ─────────────────────────────────────────────────────────────────────────────

# Scripts
SCRIPT_DATA := create_event_study_data.R
SCRIPT_ANALYSIS := run_event_study.R

# Inputs (Symlinks)
INPUT_SALES         := ../input/sales_with_ward_distances.csv
INPUT_WARD_PANEL    := ../input/ward_panel.gpkg
INPUT_BLOCKS        := ../input/census_blocks_2010.csv
INPUT_ALD_PANEL     := ../input/chicago_alderman_panel.csv
INPUT_STRICTNESS    := ../input/alderman_restrictiveness_scores_month_FEs.csv
INPUT_BG_CONTROLS   := ../input/block_group_controls.csv

ALL_INPUTS := $(INPUT_SALES) $(INPUT_WARD_PANEL) $(INPUT_BLOCKS) \
              $(INPUT_ALD_PANEL) $(INPUT_STRICTNESS) $(INPUT_BG_CONTROLS)

# Outputs
OUTPUT_BLOCK_YEAR  := ../output/sales_block_year_panel.csv
OUTPUT_BLOCK_MONTH := ../output/sales_block_month_panel.csv

# ─────────────────────────────────────────────────────────────────────────────
# Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs data analysis

all: $(OUTPUT_BLOCK_YEAR) $(OUTPUT_BLOCK_MONTH)

data: $(OUTPUT_BLOCK_YEAR) $(OUTPUT_BLOCK_MONTH)

# ─────────────────────────────────────────────────────────────────────────────
# Main Rules
# ─────────────────────────────────────────────────────────────────────────────

$(OUTPUT_BLOCK_YEAR) $(OUTPUT_BLOCK_MONTH): $(SCRIPT_DATA) $(ALL_INPUTS) | ../output
	@echo "→ [event_study_sales] Creating block-level panels..."
	$(R) $(SCRIPT_DATA)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_SALES): ../../calculate_sale_distances/output/sales_with_ward_distances.csv | ../input
	@echo "→ Linking sales data..."
	ln -sf $(abspath $<) $@

$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@echo "→ Linking ward panel..."
	ln -sf $(abspath $<) $@

$(INPUT_BLOCKS): ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	@echo "→ Linking census blocks..."
	ln -sf $(abspath $<) $@

$(INPUT_ALD_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@echo "→ Linking alderman panel..."
	ln -sf $(abspath $<) $@

$(INPUT_STRICTNESS): ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	@echo "→ Linking strictness scores..."
	ln -sf $(abspath $<) $@

$(INPUT_BG_CONTROLS): ../../create_block_group_controls/output/block_group_controls.csv | ../input
	@echo "→ Linking block group controls..."
	ln -sf $(abspath $<) $@

include ../../generic.make
