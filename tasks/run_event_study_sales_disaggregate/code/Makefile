SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters - Edit these to add/remove specifications
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES       := yearly
TREATMENT_TYPES   := continuous binary_direction
INCLUDE_HEDONICS  := TRUE FALSE
FE_TYPES          := ward_pair_side
WEIGHTINGS        := uniform triangular
BANDWIDTHS        := 500 1000 1500 2000

SCRIPT := run_event_study.R

# Inputs
INPUT_PANEL := ../input/sales_transaction_panel.parquet

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

define GEN_OUTPUTS
$(foreach freq,$(FREQUENCIES), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
	../output/event_study_disaggregate_$(freq)$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc)).pdf \
))))))
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
	../output/event_study_disaggregate_yearly_continuous_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_continuous_triangular_2000ft.pdf \
	../output/event_study_disaggregate_yearly_binary_uniform_1000ft.pdf \
	../output/event_study_disaggregate_yearly_binary_triangular_2000ft.pdf

# ─────────────────────────────────────────────────────────────────────────────
# Rule Generation
# ─────────────────────────────────────────────────────────────────────────────

define GEN_RULE
../output/event_study_disaggregate_$(freq)$(TREAT_SUFFIX_$(tt))_$(wt)_$(bw)ft$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc)).pdf: $(SCRIPT) $(INPUT_PANEL) | ../output
	@echo "→ [run_event_study_sales_disaggregate] $(freq) + $(tt) + $(wt) + $(bw)ft + fe=$(fe) + hedonics=$(hc)"
	Rscript $(SCRIPT) --treatment_type=$(tt) --time_unit=$(freq) --include_hedonics=$(hc) --fe_type=$(fe) --weighting=$(wt) --bandwidth=$(bw)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
$(foreach wt,$(WEIGHTINGS), \
$(foreach bw,$(BANDWIDTHS), \
	$(eval $(GEN_RULE)) \
))))))

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(INPUT_PANEL)

$(INPUT_PANEL): ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../input
	ln -sf $< $@

include ../../generic.make
