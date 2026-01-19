SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters - Edit these to add/remove specifications
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES       := yearly 
STACKINGS         := FALSE
STACK_TYPES       := implementation
COHORTS           := 2015
TREATMENT_TYPES   := continuous binary_direction
INCLUDE_HEDONICS  := TRUE FALSE
FE_TYPES          := ward_pair_side
WEIGHTINGS        := triangular
BANDWIDTHS        := 1000 
POST_WINDOWS      := full

SCRIPT := run_event_study.R

ALL_INPUTS := ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet

# ─────────────────────────────────────────────────────────────────────────────
# Suffix Helpers for Output Filename Generation
# ─────────────────────────────────────────────────────────────────────────────

TREAT_SUFFIX_continuous        := _continuous
TREAT_SUFFIX_binary_direction  := _binary

HEDONICS_SUFFIX_TRUE  :=
HEDONICS_SUFFIX_FALSE := _no_hedonics

FE_SUFFIX_ward_pair_side :=
FE_SUFFIX_block          := _block_fe
FE_SUFFIX_block_group    := _bg_fe

POST_SUFFIX_full  :=
POST_SUFFIX_short := _short

# ─────────────────────────────────────────────────────────────────────────────
# Generate Output Filenames
# ─────────────────────────────────────────────────────────────────────────────

define GEN_STACKED_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACK_TYPES), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach pw,$(POST_WINDOWS), \
	../output/event_study_disaggregate_$(freq)_stacked_$(st)$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc))$(POST_SUFFIX_$(pw)).pdf \
))))))))
endef

define GEN_UNSTACKED_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach coh,$(COHORTS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach pw,$(POST_WINDOWS), \
	../output/event_study_disaggregate_$(freq)_unstacked_$(coh)$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc))$(POST_SUFFIX_$(pw)).pdf \
))))))))
endef

STACKED_OUTPUTS := $(if $(filter TRUE,$(STACKINGS)),$(call GEN_STACKED_OUTPUTS),)
UNSTACKED_OUTPUTS := $(if $(filter FALSE,$(STACKINGS)),$(call GEN_UNSTACKED_OUTPUTS),)

ALL_OUTPUTS := $(STACKED_OUTPUTS) $(UNSTACKED_OUTPUTS)

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all main-specs announcement implementation all-cohorts stacked link-inputs

all: $(ALL_OUTPUTS) did-table

stacked: $(call GEN_STACKED_OUTPUTS)

main-specs: \
	../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_implementation_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_announcement_binary_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_implementation_binary_uniform_1000ft.pdf

announcement: \
	../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_announcement_binary_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2012_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2022_continuous_uniform_1000ft.pdf

implementation: \
	../output/event_study_disaggregate_yearly_stacked_implementation_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_implementation_binary_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2015_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2023_continuous_uniform_1000ft.pdf

all-cohorts: \
	../output/event_study_disaggregate_yearly_unstacked_2012_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2015_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2022_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_2023_continuous_uniform_1000ft.pdf

# ─────────────────────────────────────────────────────────────────────────────
# Rule Generation - Stacked (with stack_type parameter)
# ─────────────────────────────────────────────────────────────────────────────

define GEN_STACKED_RULE
../output/event_study_disaggregate_$(freq)_stacked_$(st)$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc))$(POST_SUFFIX_$(pw)).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) --stacked=TRUE --stack_type=$(st) --treatment_type=$(tt) --time_unit=$(freq) --include_hedonics=$(hc) --fe_type=$(fe) --weighting=$(wt) --bandwidth=$(bw) --post_window=$(pw)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACK_TYPES), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach pw,$(POST_WINDOWS), \
	$(eval $(GEN_STACKED_RULE)) \
))))))))

# ─────────────────────────────────────────────────────────────────────────────
# Rule Generation - Unstacked (with cohort parameter)
# ─────────────────────────────────────────────────────────────────────────────

define GEN_UNSTACKED_RULE
../output/event_study_disaggregate_$(freq)_unstacked_$(coh)$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc))$(POST_SUFFIX_$(pw)).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	Rscript $(SCRIPT) --stacked=FALSE --cohort=$(coh) --treatment_type=$(tt) --time_unit=$(freq) --include_hedonics=$(hc) --fe_type=$(fe) --weighting=$(wt) --bandwidth=$(bw) --post_window=$(pw)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach coh,$(COHORTS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach pw,$(POST_WINDOWS), \
	$(eval $(GEN_UNSTACKED_RULE)) \
))))))))

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking - Explicit symlink rules
# ─────────────────────────────────────────────────────────────────────────────

../input/sales_transaction_panel.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../input
	ln -sf $< $@

../input/sales_transaction_panel_announcement.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_announcement.parquet | ../input
	ln -sf $< $@

../input/sales_transaction_panel_2012.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet | ../input
	ln -sf $< $@

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf $< $@

../input/sales_transaction_panel_2022.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2022.parquet | ../input
	ln -sf $< $@

../input/sales_transaction_panel_2023.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2023.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet

# ─────────────────────────────────────────────────────────────────────────────
# DiD Table
# ─────────────────────────────────────────────────────────────────────────────

DID_SCRIPT := run_did_sales_disaggregate.R
DID_OUTPUT := ../output/did_table_sales.tex

.PHONY: did-table main

did-table: $(DID_OUTPUT)

$(DID_OUTPUT): $(DID_SCRIPT) ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet | ../output
	Rscript $(DID_SCRIPT)

main: main-specs did-table

include ../../generic.make
