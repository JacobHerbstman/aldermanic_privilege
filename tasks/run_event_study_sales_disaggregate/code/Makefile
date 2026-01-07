SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters - Edit these to add/remove specifications
# ─────────────────────────────────────────────────────────────────────────────

FREQUENCIES       := yearly quarterly
TREATMENT_TYPES   := continuous binary_direction
INCLUDE_HEDONICS  := TRUE FALSE
FE_TYPES          := ward_pair_side

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
	../output/event_study_disaggregate_$(freq)$(TREAT_SUFFIX_$(tt))$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc)).pdf \
))))
endef

ALL_OUTPUTS := $(call GEN_OUTPUTS)

# ─────────────────────────────────────────────────────────────────────────────
# Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all main block_fe bg_fe yearly quarterly hedonics no_hedonics link-inputs

all: $(ALL_OUTPUTS)

# Convenience: main specifications (ward_pair_side FE, with hedonics only)
main: $(filter ../output/event_study_disaggregate_%_continuous.pdf ../output/event_study_disaggregate_%_binary.pdf, \
      $(filter-out %_no_hedonics.pdf %_block_fe.pdf %_bg_fe.pdf, $(ALL_OUTPUTS)))

# ─────────────────────────────────────────────────────────────────────────────
# Rule Generation
# ─────────────────────────────────────────────────────────────────────────────

define GEN_RULE
../output/event_study_disaggregate_$(freq)$(TREAT_SUFFIX_$(tt))$(FE_SUFFIX_$(fe))$(HEDONICS_SUFFIX_$(hc)).pdf: $(SCRIPT) $(INPUT_PANEL) | ../output
	@echo "→ [run_event_study_sales_disaggregate] $(freq) + $(tt) + fe=$(fe) + hedonics=$(hc)"
	Rscript $(SCRIPT) --treatment_type=$(tt) --time_unit=$(freq) --include_hedonics=$(hc) --fe_type=$(fe)
endef

$(foreach freq,$(FREQUENCIES), \
$(foreach tt,$(TREATMENT_TYPES), \
$(foreach hc,$(INCLUDE_HEDONICS), \
$(foreach fe,$(FE_TYPES), \
	$(eval $(GEN_RULE)) \
))))

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(INPUT_PANEL)

$(INPUT_PANEL): ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../input
	ln -sf $< $@

include ../../generic.make
