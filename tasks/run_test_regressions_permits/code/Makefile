SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

# --- Define all output files this Makefile creates ---
TABLES := \
	../output/table_did_unbalanced.tex \
	../output/table_did_balanced.tex

PLOTS := \
	../output/event_study_unbalanced_avg_processing_time.pdf \
	../output/event_study_unbalanced_avg_reported_cost.pdf \
	../output/event_study_unbalanced_avg_total_fee.pdf \
	../output/event_study_balanced_avg_processing_time.pdf \
	../output/event_study_balanced_avg_reported_cost.pdf \
	../output/event_study_balanced_avg_total_fee.pdf

OUTPUT_FILES := $(TABLES) $(PLOTS)

# --- Define the primary script and its inputs ---
SCRIPT_FILE := test_regressions_permits.R
INPUT_UNBALANCED_PANEL := ../input/permit_regression_panel_blocks_unbalanced.csv
INPUT_BALANCED_PANEL := ../input/permit_regression_panel_blocks_balanced.csv
INPUT_FILES := $(INPUT_UNBALANCED_PANEL) $(INPUT_BALANCED_PANEL)


# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs
all: $(OUTPUT_FILES)


# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create all outputs from the R script
# This rule tells make that all files in $(OUTPUT_FILES) are created at once.
# ─────────────────────────────────────────────────────────────────────────────
$(OUTPUT_FILES): $(SCRIPT_FILE) $(INPUT_FILES) | ../output
	@echo "→ [test_regressions_permit] Running regressions, event studies, and saving outputs"
	$(R) $(SCRIPT_FILE)


# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# -----------------------------------------------------------------------------
link-inputs: $(INPUT_FILES)

$(INPUT_UNBALANCED_PANEL): ../../merge_permits_blocks/output/permit_regression_panel_blocks_unbalanced.csv | ../input
	@echo "→ [linking] Linking unbalanced regression panel"
	ln -sf $< $@

$(INPUT_BALANCED_PANEL): ../../merge_permits_blocks/output/permit_regression_panel_blocks_balanced.csv | ../input
	@echo "→ [linking] Linking balanced regression panel"
	ln -sf $< $@


# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make