SHELL := bash
include ../../shell_functions.make

# =============================================================================
# Event Study Sales Diagnostics
# =============================================================================

SCRIPT := run_diagnostics.R

# Input files
INPUT_STACKED     := ../input/sales_stacked_panel.csv
INPUT_STACKED_Q   := ../input/sales_stacked_quarterly_panel.csv
INPUT_BLOCKS      := ../input/census_blocks_2010.csv
INPUT_WARD_PANEL  := ../input/ward_panel.gpkg
INPUT_STRICTNESS  := ../input/alderman_restrictiveness_scores_month_FEs.csv

ALL_INPUTS := $(INPUT_STACKED) $(INPUT_STACKED_Q) $(INPUT_BLOCKS) $(INPUT_WARD_PANEL) $(INPUT_STRICTNESS)

# =============================================================================
# Targets
# =============================================================================

.DEFAULT_GOAL := help
.PHONY: all link-inputs

# Main outputs
SUMMARY_TEX      := ../output/sample_summary.tex
BALANCE_TEX      := ../output/covariate_balance.tex
WARD_PAIR_TEX    := ../output/ward_pair_diagnostics.tex
PRETREND_TEX     := ../output/pre_trend_tests.tex
MAP_2015         := ../output/treatment_control_map_2015.pdf
MAP_2023         := ../output/treatment_control_map_2023.pdf
PRETREND_PLOT    := ../output/pre_trend_coefficients.pdf

all: $(SUMMARY_TEX) $(BALANCE_TEX) $(WARD_PAIR_TEX) $(PRETREND_TEX) $(MAP_2015) $(MAP_2023) $(PRETREND_PLOT)

# Single script generates all outputs
$(SUMMARY_TEX) $(BALANCE_TEX) $(WARD_PAIR_TEX) $(PRETREND_TEX) $(MAP_2015) $(MAP_2023) $(PRETREND_PLOT): $(SCRIPT) $(ALL_INPUTS) | ../output
	@echo "→ Running diagnostics..."
	$(R) $(SCRIPT)

# =============================================================================
# Input Linking
# =============================================================================

link-inputs: $(ALL_INPUTS)

$(INPUT_STACKED): ../../create_event_study_sales_data/output/sales_stacked_panel.csv | ../input
	@echo "→ Linking stacked panel..."
	ln -sf $< $@

$(INPUT_STACKED_Q): ../../create_event_study_sales_data/output/sales_stacked_quarterly_panel.csv | ../input
	@echo "→ Linking stacked quarterly panel..."
	ln -sf $< $@

$(INPUT_BLOCKS): ../../create_event_study_sales_data/input/census_blocks_2010.csv | ../input
	@echo "→ Linking census blocks..."
	ln -sf $< $@

$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@echo "→ Linking ward panel..."
	ln -sf $< $@

$(INPUT_STRICTNESS): ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	@echo "→ Linking strictness scores..."
	ln -sf $< $@

include ../../generic.make
