SHELL := bash
include ../../shell_functions.make

SCRIPT := create_event_study_data.R

# Inputs
INPUT_RENTALS        := ../input/rent_with_ward_distances.parquet
INPUT_WARD_PANEL     := ../input/ward_panel.gpkg
INPUT_BLOCKS         := ../input/census_blocks_2010.csv
INPUT_TREATMENT      := ../input/block_treatment_panel.csv

ALL_INPUTS := $(INPUT_RENTALS) $(INPUT_WARD_PANEL) $(INPUT_BLOCKS) $(INPUT_TREATMENT)

# Outputs
OUTPUT_YEARLY           := ../output/rental_stacked_panel.csv
OUTPUT_QUARTERLY        := ../output/rental_stacked_quarterly_panel.csv
OUTPUT_UNSTACKED_2015   := ../output/rental_unstacked_2015_panel.csv
OUTPUT_UNSTACKED_2023   := ../output/rental_unstacked_2023_panel.csv

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUT_YEARLY) $(OUTPUT_QUARTERLY) $(OUTPUT_UNSTACKED_2015) $(OUTPUT_UNSTACKED_2023)

$(OUTPUT_YEARLY) $(OUTPUT_QUARTERLY) $(OUTPUT_UNSTACKED_2015) $(OUTPUT_UNSTACKED_2023): $(SCRIPT) $(ALL_INPUTS) | ../output
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

$(INPUT_TREATMENT): ../../create_block_treatment_panel/output/block_treatment_panel.csv | ../input
	@echo "→ Linking treatment panel..."
	ln -sf $< $@

include ../../generic.make

