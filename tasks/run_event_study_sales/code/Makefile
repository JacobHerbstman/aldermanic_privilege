SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Task Configuration
# ─────────────────────────────────────────────────────────────────────────────

SCRIPT := run_event_study.R

# All option values
FREQUENCIES     := yearly quarterly
TREATMENTS      := 2015
WEIGHTINGS      := TRUE FALSE
STACKINGS       := TRUE FALSE
TREATMENT_TYPES := continuous binary_direction

# Inputs
INPUT_BLOCK_YEAR  := ../input/sales_block_year_panel.csv
INPUT_BLOCK_MONTH := ../input/sales_block_month_panel.csv
INPUT_STACKED     := ../input/sales_stacked_panel.csv
INPUT_STACKED_Q   := ../input/sales_stacked_quarterly_panel.csv
INPUT_STACKED_M   := ../input/sales_stacked_monthly_panel.csv

ALL_INPUTS := $(INPUT_BLOCK_YEAR) $(INPUT_BLOCK_MONTH) $(INPUT_STACKED) $(INPUT_STACKED_Q) $(INPUT_STACKED_M)

# ─────────────────────────────────────────────────────────────────────────────
# Generate All Target Names
# ─────────────────────────────────────────────────────────────────────────────

# Build all combinations: run-{freq}-{treat}-{weighted}-{stacked}-{type}
ALL_TARGETS := $(foreach f,$(FREQUENCIES),\
                 $(foreach t,$(TREATMENTS),\
                   $(foreach w,$(WEIGHTINGS),\
                     $(foreach s,$(STACKINGS),\
                       $(foreach x,$(TREATMENT_TYPES),\
                         run-$(f)-$(t)-$(w)-$(s)-$(x))))))

# ─────────────────────────────────────────────────────────────────────────────
# Pattern Rule for All Targets
# ─────────────────────────────────────────────────────────────────────────────

.SECONDEXPANSION:

# Extract parameters from target name: run-{freq}-{treat}-{weighted}-{stacked}-{type}
# word 2 = freq, word 3 = treat, word 4 = weighted, word 5 = stacked, word 6 = type
$(ALL_TARGETS): run-%: $(ALL_INPUTS)
	$(R) $(SCRIPT) \
		--frequency=$(word 1,$(subst -, ,$*)) \
		--treatment_date=$(word 2,$(subst -, ,$*)) \
		--weighted=$(word 3,$(subst -, ,$*)) \
		--stacked=$(word 4,$(subst -, ,$*)) \
		--treatment_type=$(word 5,$(subst -, ,$*))

# ─────────────────────────────────────────────────────────────────────────────
# Convenience Targets
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := help
.PHONY: all-variants all-continuous all-binary all-yearly all-quarterly all-monthly clean-outputs link-inputs help

# Filter helpers
continuous_targets := $(filter %continuous,$(ALL_TARGETS))
binary_targets     := $(filter %binary_direction,$(ALL_TARGETS))
yearly_targets     := $(filter run-yearly-%,$(ALL_TARGETS))
quarterly_targets  := $(filter run-quarterly-%,$(ALL_TARGETS))
monthly_targets    := $(filter run-monthly-%,$(ALL_TARGETS))

all-variants:   $(ALL_TARGETS)
all-continuous: $(continuous_targets)
all-binary:     $(binary_targets)
all-yearly:     $(yearly_targets)
all-quarterly:  $(quarterly_targets)
all-monthly:    $(monthly_targets)

main: run-yearly-2015-TRUE-TRUE-continuous run-yearly-2015-TRUE-TRUE-binary_direction

clean-outputs:
	rm -f ../output/*.pdf ../output/*.tex

help:
	@echo "Event Study Makefile - $(words $(ALL_TARGETS)) variants"
	@echo ""
	@echo "Individual targets:"
	@echo "  make run-{freq}-{treat}-{weighted}-{stacked}-{type}"
	@echo "  Example: make run-yearly-2015-TRUE-TRUE-continuous"
	@echo ""
	@echo "Convenience targets:"
	@echo "  make all-variants    - All $(words $(ALL_TARGETS)) variants"
	@echo "  make all-continuous  - $(words $(continuous_targets)) continuous variants"
	@echo "  make all-binary      - $(words $(binary_targets)) binary direction variants"
	@echo "  make all-yearly      - $(words $(yearly_targets)) yearly variants"
	@echo "  make all-quarterly   - $(words $(quarterly_targets)) quarterly variants"
	@echo "  make all-monthly     - $(words $(monthly_targets)) monthly variants"
	@echo "  make main            - Recommended subset"
	@echo ""
	@echo "Options: freq={$(FREQUENCIES)}, treat={$(TREATMENTS)},"
	@echo "         weighted={$(WEIGHTINGS)}, stacked={$(STACKINGS)},"
	@echo "         type={$(TREATMENT_TYPES)}"

# ─────────────────────────────────────────────────────────────────────────────
# Input Linking
# ─────────────────────────────────────────────────────────────────────────────

link-inputs: $(ALL_INPUTS)

$(INPUT_BLOCK_YEAR): ../../create_event_study_sales_data/output/sales_block_year_panel.csv | ../input
	@echo "→ Linking block-year panel..."
	ln -sf $< $@

$(INPUT_BLOCK_MONTH): ../../create_event_study_sales_data/output/sales_block_month_panel.csv | ../input
	@echo "→ Linking block-month panel..."
	ln -sf $< $@

$(INPUT_STACKED): ../../create_event_study_sales_data/output/sales_stacked_panel.csv | ../input
	@echo "→ Linking stacked panel..."
	ln -sf $< $@

$(INPUT_STACKED_Q): ../../create_event_study_sales_data/output/sales_stacked_quarterly_panel.csv | ../input
	@echo "→ Linking stacked quarterly panel..."
	ln -sf $< $@

$(INPUT_STACKED_M): ../../create_event_study_sales_data/output/sales_stacked_monthly_panel.csv | ../input
	@echo "→ Linking stacked monthly panel..."
	ln -sf $< $@

include ../../generic.make
