SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES       := yearly
STACKINGS         := TRUE
TREATMENT_TYPES   := continuous binary_direction
INCLUDE_CONTROLS  := TRUE FALSE
WEIGHTINGS        := uniform
BANDWIDTHS        := 1000
SAMPLE_FILTERS    := full_sample

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

# Sample filter suffix
SAMPLE_SUFFIX_full_sample      :=
SAMPLE_SUFFIX_multifamily_only := _mf

define GEN_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach sf,$(SAMPLE_FILTERS), \
	../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))_$(tt)_$(wt)_$(bw)ft$(SAMPLE_SUFFIX_$(sf))$(CONTROLS_SUFFIX_$(ic)).pdf \
)))))))
endef

ALL_OUTPUTS := $(call GEN_OUTPUTS)

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs main-specs

all: $(ALL_OUTPUTS) did-table

# Convenience target for main specifications only
main-specs: \
	../output/event_study_disaggregate_yearly_stacked_continuous_uniform_1000ft_with_hedonics.pdf \
	../output/event_study_disaggregate_yearly_stacked_continuous_triangular_2000ft_with_hedonics.pdf \
	../output/event_study_disaggregate_yearly_stacked_binary_direction_uniform_1000ft_with_hedonics.pdf \
	../output/event_study_disaggregate_yearly_stacked_binary_direction_triangular_2000ft_with_hedonics.pdf \
	../output/event_study_disaggregate_yearly_stacked_continuous_triangular_2000ft_mf_with_hedonics.pdf \
	../output/event_study_disaggregate_yearly_stacked_binary_direction_triangular_2000ft_mf_with_hedonics.pdf

# ─────────────────────────────────────────────────────────────────────────────
# Rule Generation
# ─────────────────────────────────────────────────────────────────────────────

define GEN_RULE
../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))_$(tt)_$(wt)_$(bw)ft$(SAMPLE_SUFFIX_$(sf))$(CONTROLS_SUFFIX_$(ic)).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) --frequency=$(freq) --stacked=$(st) --treatment_type=$(tt) --include_controls=$(ic) --weighting=$(wt) --bandwidth=$(bw) --sample_filter=$(sf)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach sf,$(SAMPLE_FILTERS), \
	$(eval $(GEN_RULE)) \
)))))))

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

# =============================================================================
# DiD Table
# =============================================================================

DID_SCRIPT := run_did_rental_disaggregate.R
DID_OUTPUT := ../output/did_table_rental.tex

.PHONY: did-table main

did-table: $(DID_OUTPUT)

$(DID_OUTPUT): $(DID_SCRIPT) $(INPUT_LISTING_PANEL) | ../output
	@echo "→ [rental] Creating pooled DiD table"
	Rscript $(DID_SCRIPT)

# Main outputs including DiD table
main: main-specs did-table

include ../../generic.make
