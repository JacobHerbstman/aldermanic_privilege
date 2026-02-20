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
	# arg order: frequency stacked treatment_type include_controls weighting bandwidth sample_filter fe_type post_window
	Rscript $(SCRIPT) $(freq) $(st) $(tt) $(ic) $(wt) $(bw) $(sf) $(fe) $(pw)
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

../input/rent_with_ward_distances.parquet: ../../create_event_study_rental_data_disaggregate/input/rent_with_ward_distances.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/rental_listing_panel.parquet ../input/rental_listing_panel_2015.parquet ../input/rental_listing_panel_2023.parquet ../input/rent_with_ward_distances.parquet

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
	# arg order: bandwidth weighting fe_type
	Rscript $(DID_SCRIPT) $(DID_BANDWIDTH) $(DID_WEIGHTING) $(DID_FE_TYPE)

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

# ─────────────────────────────────────────────────────────────────────────────
# Effect Source Diagnostics
# ─────────────────────────────────────────────────────────────────────────────

EFFECT_SCRIPT := diagnose_rental_effect_sources.R
EFFECT_BANDWIDTH ?= 1000
EFFECT_WEIGHTING ?= triangular
EFFECT_SAMPLE_FILTER ?= multifamily_only
EFFECT_FE_TYPE ?= strict_pair_x_year
EFFECT_TOP_N ?= 50
EFFECT_TOP_BLOCKS ?= 200
EFFECT_TOP_BUILDINGS ?= 200

EFFECT_SUMMARY := ../output/rental_effect_source_summary.csv
EFFECT_EVENT_COEFFS := ../output/rental_effect_source_event_study_coefficients.csv
EFFECT_COUNTERFACTUAL := ../output/rental_effect_source_counterfactuals.csv
EFFECT_BOUNDARIES := ../output/rental_effect_source_boundaries.csv
EFFECT_BOUNDARIES_TOP := ../output/rental_effect_source_boundaries_top50.csv
EFFECT_WARDS := ../output/rental_effect_source_wards.csv
EFFECT_WARDS_TOP := ../output/rental_effect_source_wards_top50.csv
EFFECT_BLOCKS_TOP := ../output/rental_effect_source_blocks_top200.csv
EFFECT_BUILDINGS := ../output/rental_effect_source_buildings.csv
EFFECT_BUILDINGS_TOP := ../output/rental_effect_source_buildings_top200.csv

EFFECT_OUTPUTS := $(EFFECT_SUMMARY) $(EFFECT_EVENT_COEFFS) $(EFFECT_COUNTERFACTUAL) $(EFFECT_BOUNDARIES) $(EFFECT_BOUNDARIES_TOP) $(EFFECT_WARDS) $(EFFECT_WARDS_TOP) $(EFFECT_BLOCKS_TOP) $(EFFECT_BUILDINGS) $(EFFECT_BUILDINGS_TOP)

.PHONY: effect-sources

effect-sources: $(EFFECT_OUTPUTS)

$(EFFECT_OUTPUTS): $(EFFECT_SCRIPT) ../input/rental_listing_panel.parquet ../input/rent_with_ward_distances.parquet | ../output
	# arg order: input geo_input bandwidth weighting sample_filter fe_type top_n top_blocks top_buildings out_summary out_event_coeffs out_counterfactual out_boundaries out_boundaries_top out_wards out_wards_top out_blocks_top out_buildings out_buildings_top
	Rscript $(EFFECT_SCRIPT) \
		../input/rental_listing_panel.parquet \
		../input/rent_with_ward_distances.parquet \
		$(EFFECT_BANDWIDTH) \
		$(EFFECT_WEIGHTING) \
		$(EFFECT_SAMPLE_FILTER) \
		$(EFFECT_FE_TYPE) \
		$(EFFECT_TOP_N) \
		$(EFFECT_TOP_BLOCKS) \
		$(EFFECT_TOP_BUILDINGS) \
		$(EFFECT_SUMMARY) \
		$(EFFECT_EVENT_COEFFS) \
		$(EFFECT_COUNTERFACTUAL) \
		$(EFFECT_BOUNDARIES) \
		$(EFFECT_BOUNDARIES_TOP) \
		$(EFFECT_WARDS) \
		$(EFFECT_WARDS_TOP) \
		$(EFFECT_BLOCKS_TOP) \
		$(EFFECT_BUILDINGS) \
		$(EFFECT_BUILDINGS_TOP)

main: main-specs did-table

include ../../generic.make
