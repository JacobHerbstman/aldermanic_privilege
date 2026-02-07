SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES       := yearly
STACKINGS         := TRUE FALSE
TREATMENT_TYPES   := continuous continuous_split
INCLUDE_CONTROLS  := TRUE FALSE
WEIGHTINGS        := triangular uniform
BANDWIDTHS        := 500 1000
SAMPLE_FILTERS    := multifamily_only
POST_WINDOWS      := short full
FE_TYPES          ?= strict_pair_x_year

SCRIPT := run_event_study_disaggregate.R

ALL_INPUTS := ../input/rental_listing_panel.parquet ../input/rental_listing_panel_2015.parquet ../input/rental_listing_panel_2023.parquet

# ─────────────────────────────────────────────────────────────────────────────
# Generate Output Filenames
# ─────────────────────────────────────────────────────────────────────────────

STACK_PREFIX_TRUE  := stacked
STACK_PREFIX_FALSE := unstacked

CONTROLS_SUFFIX_TRUE  :=
CONTROLS_SUFFIX_FALSE := _no_hedonics

SAMPLE_SUFFIX_full_sample      :=
SAMPLE_SUFFIX_multifamily_only := _mf

POST_SUFFIX_full  :=
POST_SUFFIX_short := _short

FE_SUFFIX_strict_pair_x_year :=
FE_SUFFIX_pair_trend_plus_year := _pairtrend
FE_SUFFIX_side_plus_year := _yearfe

define GEN_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach sf,$(SAMPLE_FILTERS), \
$(foreach fe,$(FE_TYPES), \
$(foreach pw,$(POST_WINDOWS), \
	../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))_$(tt)_$(wt)_$(bw)ft$(SAMPLE_SUFFIX_$(sf))$(CONTROLS_SUFFIX_$(ic))$(FE_SUFFIX_$(fe))$(POST_SUFFIX_$(pw)).pdf \
)))))))))
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
../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))_$(tt)_$(wt)_$(bw)ft$(SAMPLE_SUFFIX_$(sf))$(CONTROLS_SUFFIX_$(ic))$(FE_SUFFIX_$(fe))$(POST_SUFFIX_$(pw)).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) --frequency=$(freq) --stacked=$(st) --treatment_type=$(tt) --include_controls=$(ic) --weighting=$(wt) --bandwidth=$(bw) --sample_filter=$(sf) --fe_type=$(fe) --post_window=$(pw)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach ic,$(INCLUDE_CONTROLS), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach sf,$(SAMPLE_FILTERS), \
$(foreach fe,$(FE_TYPES), \
$(foreach pw,$(POST_WINDOWS), \
	$(eval $(GEN_RULE)) \
)))))))))

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
DID_FE_TYPE ?= strict_pair_x_year
DID_WEIGHTING ?= triangular
DID_BANDWIDTH ?= 1000

DID_SUFFIX_strict_pair_x_year :=
DID_SUFFIX_pair_trend_plus_year := _pairtrend
DID_SUFFIX_side_plus_year := _yearfe

DID_OUTPUT := ../output/did_table_rental$(DID_SUFFIX_$(DID_FE_TYPE)).tex

.PHONY: did-table did-table-strict did-table-pairtrend did-table-yearfe did-table-all main

did-table: $(DID_OUTPUT)

$(DID_OUTPUT): $(DID_SCRIPT) ../input/rental_listing_panel.parquet | ../output
	Rscript $(DID_SCRIPT) --bandwidth=$(DID_BANDWIDTH) --weighting=$(DID_WEIGHTING) --fe_type=$(DID_FE_TYPE)

did-table-strict:
	$(MAKE) did-table DID_FE_TYPE=strict_pair_x_year

did-table-pairtrend:
	$(MAKE) did-table DID_FE_TYPE=pair_trend_plus_year

did-table-yearfe:
	$(MAKE) did-table DID_FE_TYPE=side_plus_year

did-table-all:
	$(MAKE) did-table-strict
	$(MAKE) did-table-pairtrend
	$(MAKE) did-table-yearfe

main: main-specs did-table

include ../../generic.make
