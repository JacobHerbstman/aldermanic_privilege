SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters - Edit these to add/remove specifications
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES       := yearly
STACKINGS         := FALSE
TREATMENT_TYPES   := continuous binary_direction
INCLUDE_HEDONICS  := TRUE FALSE
FE_TYPES          := ward_pair_side
WEIGHTINGS        := uniform triangular
BANDWIDTHS        := 1000 1500 2000

SCRIPT := run_event_study.R

# Inputs
INPUT_PANEL       := ../input/sales_transaction_panel.parquet
INPUT_PANEL_2015  := ../input/sales_transaction_panel_2015.parquet
INPUT_PANEL_2023  := ../input/sales_transaction_panel_2023.parquet

ALL_INPUTS := $(INPUT_PANEL) $(INPUT_PANEL_2015) $(INPUT_PANEL_2023)

# ─────────────────────────────────────────────────────────────────────────────
# Suffix Helpers for Output Filename Generation
# ─────────────────────────────────────────────────────────────────────────────

# Stacking suffix
STACK_PREFIX_TRUE  := stacked
STACK_PREFIX_FALSE := unstacked

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

define GEN_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
	../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc)).pdf \
)))))))
endef

ALL_OUTPUTS := $(call GEN_OUTPUTS)

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all main-specs link-inputs

all: $(ALL_OUTPUTS)

# Convenience target for main specifications only
main-specs: \
	../output/event_study_disaggregate_yearly_stacked_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_continuous_triangular_2000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_binary_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_stacked_binary_triangular_2000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_continuous_triangular_2000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_binary_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_unstacked_binary_triangular_2000ft.pdf

# ─────────────────────────────────────────────────────────────────────────────
# Rule Generation
# ─────────────────────────────────────────────────────────────────────────────

define GEN_RULE
../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc)).pdf: $(SCRIPT) $(ALL_INPUTS) | ../output
	@echo "→ [run_event_study_sales_disaggregate] $(freq) + stacked=$(st) + $(tt) + $(wt) + $(bw)ft + fe=$(fe) + hedonics=$(hc)"
	Rscript $(SCRIPT) --stacked=$(st) --treatment_type=$(tt) --time_unit=$(freq) --include_hedonics=$(hc) --fe_type=$(fe) --weighting=$(wt) --bandwidth=$(bw)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach st,$(STACKINGS), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
	$(eval $(GEN_RULE)) \
)))))))

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_PANEL): ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../input
	ln -sf $< $@

$(INPUT_PANEL_2015): ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf $< $@

$(INPUT_PANEL_2023): ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2023.parquet | ../input
	ln -sf $< $@

include ../../generic.make
