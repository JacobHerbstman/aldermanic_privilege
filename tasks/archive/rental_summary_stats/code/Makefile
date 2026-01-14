SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Config
# ─────────────────────────────────────────────────────────────────────────────
# Outputs
OUTPUT_CSV  := ../output/rental_summary_stats.csv
OUTPUT_TEX  := ../output/rental_summary_stats.tex
OUTPUT_MAP1 := ../output/map_rental_coverage_density.pdf
OUTPUT_MAP2 := ../output/map_rent_growth_ward.pdf
OUTPUT_MAP3 := ../output/map_luxury_amenities_ward.pdf
OUTPUT_MAP4 := ../output/map_rent_growth_ward_2021_2025.pdf

# Scripts
SCRIPT_STATS := rental_summary_stats.R
SCRIPT_MAPS  := summary_maps.R

# Inputs
INPUT_DATA  := ../input/chicago_rent_panel.parquet
INPUT_WARDS := ../input/ward_panel.gpkg

# ─────────────────────────────────────────────────────────────────────────────
# Goals
# ─────────────────────────────────────────────────────────────────────────────
.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUT_CSV) $(OUTPUT_TEX) $(OUTPUT_MAP1) $(OUTPUT_MAP4)

# ─────────────────────────────────────────────────────────────────────────────
# Rules
# ─────────────────────────────────────────────────────────────────────────────

# 1. Summary Stats Table
$(OUTPUT_CSV) $(OUTPUT_TEX): $(SCRIPT_STATS) $(INPUT_DATA) | ../output
	@echo "→ [Rental Stats] Generating summary statistics table..."
	$(R) $(SCRIPT_STATS)

# 2. Summary Maps
# Note: Since one script generates all maps, they are all targets of the same rule.
$(OUTPUT_MAP1) $(OUTPUT_MAP2) $(OUTPUT_MAP3) $(OUTPUT_MAP4): $(SCRIPT_MAPS) $(INPUT_DATA) $(INPUT_WARDS) | ../output
	@echo "→ [Rental Stats] Generating summary maps..."
	$(R) $(SCRIPT_MAPS)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────
$(INPUT_DATA): ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	@echo "→ [Rental Stats] Linking rent panel..."
	ln -sf $< $@

$(INPUT_WARDS): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@echo "→ [Rental Stats] Linking ward boundaries..."
	ln -sf $< $@

link-inputs: $(INPUT_DATA) $(INPUT_WARDS)

# ─────────────────────────────────────────────────────────────────────────────
# Generic
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make