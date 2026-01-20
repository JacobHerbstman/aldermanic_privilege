SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

SCRIPT := combine_tables.R

RENTAL_COEF := ../input/did_coefficients_rental.csv
SALES_COEF := ../input/did_coefficients_sales.csv

ALL_INPUTS := $(RENTAL_COEF) $(SALES_COEF)

MAIN_OUTPUT := ../output/did_table_combined.tex
APPENDIX_OUTPUT := ../output/did_table_sales_announcement.tex

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs clean

all: $(MAIN_OUTPUT)

$(MAIN_OUTPUT) $(APPENDIX_OUTPUT): $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT)

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

$(RENTAL_COEF): ../../run_event_study_rental_disaggregate/output/did_coefficients_rental.csv | ../input
	ln -sf $< $@

$(SALES_COEF): ../../run_event_study_sales_disaggregate/output/did_coefficients_sales.csv | ../input
	ln -sf $< $@

link-inputs: $(ALL_INPUTS)

# ─────────────────────────────────────────────────────────────────────────────
# Directory Creation
# ─────────────────────────────────────────────────────────────────────────────

include ../../generic.make
