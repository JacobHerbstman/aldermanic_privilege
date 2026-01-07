SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES       := yearly quarterly
STACKINGS         := TRUE
TREATMENT_TYPES   := continuous binary_direction
INCLUDE_CONTROLS  := TRUE FALSE

SCRIPT := run_event_study_disaggregate.R

# Inputs
INPUT_LISTING_PANEL  := ../input/rental_listing_panel.parquet
INPUT_LISTING_2015   := ../input/rental_listing_panel_2015.parquet
INPUT_LISTING_2023   := ../input/rental_listing_panel_2023.parquet

ALL_INPUTS := $(INPUT_LISTING_PANEL) $(INPUT_LISTING_2015) $(INPUT_LISTING_2023)

# ─────────────────────────────────────────────────────────────────────────────
# Generate Output Filenames
# ─────────────────────────────────────────────────────────────────────────────

# Helper for stacked prefix
STACK_PREFIX_TRUE  := stacked
STACK_PREFIX_FALSE := unstacked

# Helper for controls suffix
CONTROLS_SUFFIX_TRUE  := _with_hedonics
CONTROLS_SUFFIX_FALSE :=

define GEN_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
	../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))_$(tt)$(CONTROLS_SUFFIX_$(ic)).pdf \
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
../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))_$(tt)$(CONTROLS_SUFFIX_$(ic)).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) --frequency=$(freq) --stacked=$(st) --treatment_type=$(tt) --include_controls=$(ic)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
	$(eval $(GEN_RULE)) \
))))

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_LISTING_PANEL): ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../input
	ln -sf $< $@

$(INPUT_LISTING_2015): ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2015.parquet | ../input
	ln -sf $< $@

$(INPUT_LISTING_2023): ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2023.parquet | ../input
	ln -sf $< $@

include ../../generic.make

