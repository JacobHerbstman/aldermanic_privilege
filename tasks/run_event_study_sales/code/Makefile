SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES       := yearly quarterly
WEIGHTINGS        := TRUE FALSE
STACKINGS         := TRUE FALSE
TREATMENT_TYPES   := continuous binary_direction
INCLUDE_CONTROLS  := FALSE
FE_TYPES          := ward_pair_side block

SCRIPT := run_event_study.R

# Inputs
INPUT_BLOCK_YEAR     := ../input/sales_block_year_panel.csv
INPUT_STACKED        := ../input/sales_stacked_panel.csv
INPUT_STACKED_Q      := ../input/sales_stacked_quarterly_panel.csv
INPUT_UNSTACKED_2015 := ../input/sales_unstacked_2015_panel.csv

ALL_INPUTS := $(INPUT_BLOCK_YEAR) $(INPUT_STACKED) $(INPUT_STACKED_Q) $(INPUT_UNSTACKED_2015)

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

# Helper for FE suffix (block adds suffix, ward_pair_side is default so no suffix)
FE_SUFFIX_block           := _block_fe
FE_SUFFIX_ward_pair_side  :=

define GEN_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
$(foreach fe,$(FE_TYPES), \
	../output/event_study_$(freq)_$(WEIGHT_PREFIX_$(wt))_$(STACK_PREFIX_$(st))_$(tt)$(CONTROLS_SUFFIX_$(ic))$(FE_SUFFIX_$(fe)).pdf \
))))))
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
../output/event_study_$(freq)_$(WEIGHT_PREFIX_$(wt))_$(STACK_PREFIX_$(st))_$(tt)$(CONTROLS_SUFFIX_$(ic))$(FE_SUFFIX_$(fe)).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) --frequency=$(freq) --weighted=$(wt) --stacked=$(st) --treatment_type=$(tt) --include_controls=$(ic) --fe_type=$(fe)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
$(foreach fe,$(FE_TYPES), \
	$(eval $(GEN_RULE)) \
))))))

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_BLOCK_YEAR): ../../create_event_study_sales_data/output/sales_block_year_panel.csv | ../input
	ln -sf $< $@

$(INPUT_STACKED): ../../create_event_study_sales_data/output/sales_stacked_panel.csv | ../input
	ln -sf $< $@

$(INPUT_STACKED_Q): ../../create_event_study_sales_data/output/sales_stacked_quarterly_panel.csv | ../input
	ln -sf $< $@

$(INPUT_UNSTACKED_2015): ../../create_event_study_sales_data/output/sales_unstacked_2015_panel.csv | ../input
	ln -sf $< $@

include ../../generic.make
