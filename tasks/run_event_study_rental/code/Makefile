SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES     := yearly quarterly
WEIGHTINGS      := TRUE FALSE
STACKINGS       := TRUE FALSE
TREATMENT_TYPES := continuous binary_direction

SCRIPT := run_event_study.R

# Inputs
INPUT_STACKED   := ../input/rental_stacked_panel.csv
INPUT_STACKED_Q := ../input/rental_stacked_quarterly_panel.csv

ALL_INPUTS := $(INPUT_STACKED) $(INPUT_STACKED_Q)

# ─────────────────────────────────────────────────────────────────────────────
# Generate Output Filenames
# ─────────────────────────────────────────────────────────────────────────────

# Helper for weighted prefix
WEIGHT_PREFIX_TRUE  := weighted
WEIGHT_PREFIX_FALSE := unweighted

# Helper for stacked prefix
STACK_PREFIX_TRUE  := stacked
STACK_PREFIX_FALSE := unstacked

define GEN_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
	../output/event_study_$(freq)_$(WEIGHT_PREFIX_$(wt))_$(STACK_PREFIX_$(st))_$(tt).pdf \
))))
endef

ALL_OUTPUTS := $(call GEN_OUTPUTS)

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(ALL_OUTPUTS)

# ─────────────────────────────────────────────────────────────────────────────
# Rule Generation
# ─────────────────────────────────────────────────────────────────────────────

define GEN_RULE
../output/event_study_$(freq)_$(WEIGHT_PREFIX_$(wt))_$(STACK_PREFIX_$(st))_$(tt).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	$(R) $(SCRIPT) --frequency=$(freq) --weighted=$(wt) --stacked=$(st) --treatment_type=$(tt)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
	$(eval $(GEN_RULE)) \
))))

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_STACKED): ../../create_event_study_rental_data/output/rental_stacked_panel.csv | ../input
	ln -sf $< $@

$(INPUT_STACKED_Q): ../../create_event_study_rental_data/output/rental_stacked_quarterly_panel.csv | ../input
	ln -sf $< $@

include ../../generic.make
