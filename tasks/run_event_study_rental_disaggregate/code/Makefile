SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES       := yearly
STACKINGS         := TRUE
TREATMENT_TYPES   := continuous binary_direction
INCLUDE_CONTROLS  := TRUE FALSE
WEIGHTINGS        := triangular
BANDWIDTHS        := 500 1000
SAMPLE_FILTERS    := full_sample
POST_WINDOWS      := short

SCRIPT := run_event_study_disaggregate.R

ALL_INPUTS := ../input/rental_listing_panel.parquet ../input/rental_listing_panel_2015.parquet ../input/rental_listing_panel_2023.parquet

# ─────────────────────────────────────────────────────────────────────────────
# Generate Output Filenames
# ─────────────────────────────────────────────────────────────────────────────

STACK_PREFIX_TRUE  := stacked
STACK_PREFIX_FALSE := unstacked

CONTROLS_SUFFIX_TRUE  := _with_hedonics
CONTROLS_SUFFIX_FALSE :=

SAMPLE_SUFFIX_full_sample      :=
SAMPLE_SUFFIX_multifamily_only := _mf

POST_SUFFIX_full  :=
POST_SUFFIX_short := _short

define GEN_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach sf,$(SAMPLE_FILTERS), \
$(foreach pw,$(POST_WINDOWS), \
	../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))_$(tt)_$(wt)_$(bw)ft$(SAMPLE_SUFFIX_$(sf))$(CONTROLS_SUFFIX_$(ic))$(POST_SUFFIX_$(pw)).pdf \
))))))))
endef

ALL_OUTPUTS := $(call GEN_OUTPUTS)

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all link-inputs main-specs

all: $(ALL_OUTPUTS) did-table

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
../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))_$(tt)_$(wt)_$(bw)ft$(SAMPLE_SUFFIX_$(sf))$(CONTROLS_SUFFIX_$(ic))$(POST_SUFFIX_$(pw)).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) --frequency=$(freq) --stacked=$(st) --treatment_type=$(tt) --include_controls=$(ic) --weighting=$(wt) --bandwidth=$(bw) --sample_filter=$(sf) --post_window=$(pw)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach sf,$(SAMPLE_FILTERS), \
$(foreach pw,$(POST_WINDOWS), \
	$(eval $(GEN_RULE)) \
))))))))

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking - Explicit symlink rules
# ─────────────────────────────────────────────────────────────────────────────

../input/rental_listing_panel.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../input
	ln -sf $< $@

../input/rental_listing_panel_2015.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2015.parquet | ../input
	ln -sf $< $@

../input/rental_listing_panel_2023.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2023.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/rental_listing_panel.parquet ../input/rental_listing_panel_2015.parquet ../input/rental_listing_panel_2023.parquet

# ─────────────────────────────────────────────────────────────────────────────
# DiD Table
# ─────────────────────────────────────────────────────────────────────────────

DID_SCRIPT := run_did_rental_disaggregate.R
DID_OUTPUT := ../output/did_table_rental.tex

.PHONY: did-table main

did-table: $(DID_OUTPUT)

$(DID_OUTPUT): $(DID_SCRIPT) ../input/rental_listing_panel.parquet | ../output
	Rscript $(DID_SCRIPT)

main: main-specs did-table

include ../../generic.make
