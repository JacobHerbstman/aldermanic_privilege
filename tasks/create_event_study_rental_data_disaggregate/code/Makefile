SHELL := bash
include ../../shell_functions.make

SCRIPT := create_listing_panel.R

# Inputs
INPUT_RENTALS        := ../input/rent_with_ward_distances.parquet
INPUT_BLOCKS         := ../input/census_blocks_2010.csv
INPUT_TREATMENT      := ../input/block_treatment_panel.csv

ALL_INPUTS := $(INPUT_RENTALS) $(INPUT_BLOCKS) $(INPUT_TREATMENT)

# Outputs
OUTPUT_LISTING_PANEL := ../output/rental_listing_panel.parquet

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUT_LISTING_PANEL)

$(OUTPUT_LISTING_PANEL): $(SCRIPT) $(ALL_INPUTS) | ../output
	$(R) $(SCRIPT)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_RENTALS): ../../calculate_rent_distances/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf $< $@

$(INPUT_BLOCKS): ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

$(INPUT_TREATMENT): ../../create_block_treatment_panel/output/block_treatment_panel.csv | ../input
	@echo "→ Linking treatment panel..."
	ln -sf $< $@

include ../../generic.make

