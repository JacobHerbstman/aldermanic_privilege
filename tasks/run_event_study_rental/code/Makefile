SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES       := yearly quarterly
WEIGHTINGS        := TRUE FALSE
STACKINGS         := TRUE
TREATMENT_TYPES   := continuous binary_direction
INCLUDE_CONTROLS  := TRUE FALSE

SCRIPT := run_event_study.R

# Inputs
INPUT_STACKED        := ../input/rental_stacked_panel.csv
INPUT_STACKED_Q      := ../input/rental_stacked_quarterly_panel.csv
INPUT_UNSTACKED_2015 := ../input/rental_unstacked_2015_panel.csv

ALL_INPUTS := $(INPUT_STACKED) $(INPUT_STACKED_Q) $(INPUT_UNSTACKED_2015)

# ─────────────────────────────────────────────────────────────────────────────
# Generate Output Filenames
# ─────────────────────────────────────────────────────────────────────────────

# Helper for weighted prefix
WEIGHT_PREFIX_TRUE  := weighted
WEIGHT_PREFIX_FALSE := unweighted

# Helper for stacked prefix
STACK_PREFIX_TRUE  := stacked
STACK_PREFIX_FALSE := unstacked

# Helper for controls suffix
CONTROLS_SUFFIX_TRUE  := _with_controls
CONTROLS_SUFFIX_FALSE :=

define GEN_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
	../output/event_study_$(freq)_$(WEIGHT_PREFIX_$(wt))_$(STACK_PREFIX_$(st))_$(tt)$(CONTROLS_SUFFIX_$(ic)).pdf \
)))))
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
../output/event_study_$(freq)_$(WEIGHT_PREFIX_$(wt))_$(STACK_PREFIX_$(st))_$(tt)$(CONTROLS_SUFFIX_$(ic)).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) --frequency=$(freq) --weighted=$(wt) --stacked=$(st) --treatment_type=$(tt) --include_controls=$(ic)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
	$(eval $(GEN_RULE)) \
)))))

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_STACKED): ../../create_event_study_rental_data/output/rental_stacked_panel.csv | ../input
	ln -sf $< $@

$(INPUT_STACKED_Q): ../../create_event_study_rental_data/output/rental_stacked_quarterly_panel.csv | ../input
	ln -sf $< $@

$(INPUT_UNSTACKED_2015): ../../create_event_study_rental_data/output/rental_unstacked_2015_panel.csv | ../input
	ln -sf $< $@

include ../../generic.make
