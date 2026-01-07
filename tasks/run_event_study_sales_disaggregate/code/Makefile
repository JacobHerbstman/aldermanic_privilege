SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters - Edit these to add/remove specifications
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES       := yearly
STACKINGS         := FALSE
STACK_TYPES       := announcement implementation
COHORTS           := 2012 2015
TREATMENT_TYPES   := continuous binary_direction
INCLUDE_HEDONICS  := TRUE FALSE
FE_TYPES          := ward_pair_side
WEIGHTINGS        := uniform
BANDWIDTHS        := 1000 

SCRIPT := run_event_study.R

# Inputs - Stacked panels
INPUT_PANEL_IMPL  := ../input/sales_transaction_panel.parquet
INPUT_PANEL_ANN   := ../input/sales_transaction_panel_announcement.parquet

# Inputs - Individual cohort panels
INPUT_PANEL_2012  := ../input/sales_transaction_panel_2012.parquet
INPUT_PANEL_2015  := ../input/sales_transaction_panel_2015.parquet
INPUT_PANEL_2022  := ../input/sales_transaction_panel_2022.parquet
INPUT_PANEL_2023  := ../input/sales_transaction_panel_2023.parquet

ALL_INPUTS := $(INPUT_PANEL_IMPL) $(INPUT_PANEL_ANN) $(INPUT_PANEL_2012) $(INPUT_PANEL_2015) $(INPUT_PANEL_2022) $(INPUT_PANEL_2023)

# ─────────────────────────────────────────────────────────────────────────────
# Suffix Helpers for Output Filename Generation
# ─────────────────────────────────────────────────────────────────────────────

# Treatment type suffix
TREAT_SUFFIX_continuous        := _continuous
TREAT_SUFFIX_binary_direction  := _binary

# Hedonics suffix
HEDONICS_SUFFIX_TRUE  :=
HEDONICS_SUFFIX_FALSE := _no_hedonics

# FE type suffix
FE_SUFFIX_ward_pair_side :=
FE_SUFFIX_block          := _block_fe
FE_SUFFIX_block_group    := _bg_fe

# ─────────────────────────────────────────────────────────────────────────────
# Generate Output Filenames
# ─────────────────────────────────────────────────────────────────────────────

# Stacked outputs (with stack_type: announcement or implementation)
define GEN_STACKED_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACK_TYPES), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
	../output/event_study_disaggregate_$(freq)_stacked_$(st)$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc)).pdf \
)))))))
endef

# Unstacked outputs (with cohort: 2012, 2015, 2022, 2023)
define GEN_UNSTACKED_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach coh,$(COHORTS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
	../output/event_study_disaggregate_$(freq)_unstacked_$(coh)$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc)).pdf \
)))))))
endef

# Conditionally include stacked/unstacked based on STACKINGS parameter
STACKED_OUTPUTS := $(if $(filter TRUE,$(STACKINGS)),$(call GEN_STACKED_OUTPUTS),)
UNSTACKED_OUTPUTS := $(if $(filter FALSE,$(STACKINGS)),$(call GEN_UNSTACKED_OUTPUTS),)

ALL_OUTPUTS := $(STACKED_OUTPUTS) $(UNSTACKED_OUTPUTS)

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all main-specs announcement implementation all-cohorts stacked link-inputs

all: $(ALL_OUTPUTS) did-table

# Just stacked specifications (both announcement and implementation)
stacked: $(call GEN_STACKED_OUTPUTS)

# Main specifications - compare announcement vs implementation timing
main-specs: \
	../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_implementation_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_announcement_binary_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_implementation_binary_uniform_1000ft.pdf

# Announcement timing only (stacked 2012+2022 and individual cohorts)
announcement: \
	../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_announcement_binary_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2012_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2022_continuous_uniform_1000ft.pdf

# Implementation timing only (stacked 2015+2023 and individual cohorts)
implementation: \
	../output/event_study_disaggregate_yearly_stacked_implementation_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_implementation_binary_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2015_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2023_continuous_uniform_1000ft.pdf

# Compare all four individual cohorts
all-cohorts: \
	../output/event_study_disaggregate_yearly_unstacked_2012_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2015_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2022_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2023_continuous_uniform_1000ft.pdf

# ─────────────────────────────────────────────────────────────────────────────
# Rule Generation - Stacked (with stack_type parameter)
# ─────────────────────────────────────────────────────────────────────────────

define GEN_STACKED_RULE
../output/event_study_disaggregate_$(freq)_stacked_$(st)$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc)).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	@echo "→ [sales] stacked $(st) + $(freq) + $(tt) + $(wt) + $(bw)ft + fe=$(fe) + hedonics=$(hc)"
	Rscript $(SCRIPT) --stacked=TRUE --stack_type=$(st) --treatment_type=$(tt) --time_unit=$(freq) --include_hedonics=$(hc) --fe_type=$(fe) --weighting=$(wt) --bandwidth=$(bw)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACK_TYPES), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
	$(eval $(GEN_STACKED_RULE)) \
)))))))

# ─────────────────────────────────────────────────────────────────────────────
# Rule Generation - Unstacked (with cohort parameter)
# ─────────────────────────────────────────────────────────────────────────────

define GEN_UNSTACKED_RULE
../output/event_study_disaggregate_$(freq)_unstacked_$(coh)$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc)).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	@echo "→ [sales] unstacked $(coh) + $(freq) + $(tt) + $(wt) + $(bw)ft + fe=$(fe) + hedonics=$(hc)"
	Rscript $(SCRIPT) --stacked=FALSE --cohort=$(coh) --treatment_type=$(tt) --time_unit=$(freq) --include_hedonics=$(hc) --fe_type=$(fe) --weighting=$(wt) --bandwidth=$(bw)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach coh,$(COHORTS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
	$(eval $(GEN_UNSTACKED_RULE)) \
)))))))

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_PANEL_IMPL): ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../input
	ln -sf $< $@

$(INPUT_PANEL_ANN): ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_announcement.parquet | ../input
	ln -sf $< $@

$(INPUT_PANEL_2012): ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet | ../input
	ln -sf $< $@

$(INPUT_PANEL_2015): ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf $< $@

$(INPUT_PANEL_2022): ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2022.parquet | ../input
	ln -sf $< $@

$(INPUT_PANEL_2023): ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2023.parquet | ../input
	ln -sf $< $@

# =============================================================================
# DiD Table
# =============================================================================

DID_SCRIPT := run_did_sales_disaggregate.R
DID_OUTPUT := ../output/did_table_sales.tex

.PHONY: did-table main

did-table: $(DID_OUTPUT)

$(DID_OUTPUT): $(DID_SCRIPT) $(INPUT_PANEL_IMPL) $(INPUT_PANEL_ANN) | ../output
	@echo "→ [sales] Creating pooled DiD table"
	Rscript $(DID_SCRIPT)

# Main outputs including DiD table
main: main-specs did-table

include ../../generic.make
