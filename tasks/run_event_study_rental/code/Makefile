SHELL := bash
include ../../shell_functions.make

SCRIPT := run_event_study.R

# Inputs
INPUT_STACKED   := ../input/rental_stacked_panel.csv
INPUT_STACKED_Q := ../input/rental_stacked_quarterly_panel.csv

ALL_INPUTS := $(INPUT_STACKED) $(INPUT_STACKED_Q)

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

# All outputs: yearly & quarterly × weighted/unweighted × stacked/unstacked × continuous/binary
all: $(ALL_INPUTS)
	@echo "=== Running all rental event study specifications ==="
	$(R) $(SCRIPT) --frequency=yearly    --weighted=TRUE  --stacked=TRUE  --treatment_type=continuous
	$(R) $(SCRIPT) --frequency=yearly    --weighted=TRUE  --stacked=TRUE  --treatment_type=binary_direction
	$(R) $(SCRIPT) --frequency=yearly    --weighted=TRUE  --stacked=FALSE --treatment_type=continuous
	$(R) $(SCRIPT) --frequency=yearly    --weighted=TRUE  --stacked=FALSE --treatment_type=binary_direction
	$(R) $(SCRIPT) --frequency=yearly    --weighted=FALSE --stacked=TRUE  --treatment_type=continuous
	$(R) $(SCRIPT) --frequency=yearly    --weighted=FALSE --stacked=TRUE  --treatment_type=binary_direction
	$(R) $(SCRIPT) --frequency=yearly    --weighted=FALSE --stacked=FALSE --treatment_type=continuous
	$(R) $(SCRIPT) --frequency=yearly    --weighted=FALSE --stacked=FALSE --treatment_type=binary_direction
	$(R) $(SCRIPT) --frequency=quarterly --weighted=TRUE  --stacked=TRUE  --treatment_type=continuous
	$(R) $(SCRIPT) --frequency=quarterly --weighted=TRUE  --stacked=TRUE  --treatment_type=binary_direction
	$(R) $(SCRIPT) --frequency=quarterly --weighted=TRUE  --stacked=FALSE --treatment_type=continuous
	$(R) $(SCRIPT) --frequency=quarterly --weighted=TRUE  --stacked=FALSE --treatment_type=binary_direction
	$(R) $(SCRIPT) --frequency=quarterly --weighted=FALSE --stacked=TRUE  --treatment_type=continuous
	$(R) $(SCRIPT) --frequency=quarterly --weighted=FALSE --stacked=TRUE  --treatment_type=binary_direction
	$(R) $(SCRIPT) --frequency=quarterly --weighted=FALSE --stacked=FALSE --treatment_type=continuous
	$(R) $(SCRIPT) --frequency=quarterly --weighted=FALSE --stacked=FALSE --treatment_type=binary_direction

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_STACKED): ../../create_event_study_rental_data/output/rental_stacked_panel.csv | ../input
	ln -sf $< $@

$(INPUT_STACKED_Q): ../../create_event_study_rental_data/output/rental_stacked_quarterly_panel.csv | ../input
	ln -sf $< $@

include ../../generic.make
