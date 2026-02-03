SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters - Set these to run specific specification(s)
# You can specify multiple values separated by spaces to loop through them
# 
# Examples:
#   make                                              # Default spec
#   make CA_FE=TRUE                                   # CA FE enabled
#   make INCLUDE_PORCH="TRUE FALSE"                   # Both porch specs
#   make all-specs                                    # All combinations
# ─────────────────────────────────────────────────────────────────────────────

PERMIT_TYPE_FE ?= FALSE
REVIEW_TYPE_FE ?= TRUE
INCLUDE_PORCH  ?= TRUE FALSE
CA_FE          ?= TRUE
TWO_STAGE      ?= TRUE

SCRIPT := create_uncertainty_index.R
INPUT := ../input/permits_for_uncertainty_index.csv

# ─────────────────────────────────────────────────────────────────────────────
# Generate all output filenames from parameter combinations
# ─────────────────────────────────────────────────────────────────────────────

# Helper to generate 2stage suffix
twostage_suffix = $(if $(filter TRUE,$(1)),_2stage,)

define GEN_OUTPUTS
$(foreach ptfe,$(PERMIT_TYPE_FE), \
$(foreach rtfe,$(REVIEW_TYPE_FE), \
$(foreach porch,$(INCLUDE_PORCH), \
$(foreach cafe,$(CA_FE), \
$(foreach ts,$(TWO_STAGE), \
	../output/alderman_uncertainty_index_ptfe$(ptfe)_rtfe$(rtfe)_porch$(porch)_cafe$(cafe)$(call twostage_suffix,$(ts)).csv \
)))))
endef

ALL_OUTPUTS := $(call GEN_OUTPUTS)

# ─────────────────────────────────────────────────────────────────────────────
# Main Target - Builds all combinations of specified parameters
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all all-specs link-inputs

all: $(ALL_OUTPUTS)

# ─────────────────────────────────────────────────────────────────────────────
# Rule Generation - Creates a rule for each parameter combination
# ─────────────────────────────────────────────────────────────────────────────

define GEN_RULE
../output/alderman_uncertainty_index_ptfe$(ptfe)_rtfe$(rtfe)_porch$(porch)_cafe$(cafe)$(call twostage_suffix,$(ts)).csv: $(SCRIPT) $(INPUT) | ../output
	Rscript $(SCRIPT) --permit_type_fe=$(ptfe) --review_type_fe=$(rtfe) --include_porch=$(porch) --ca_fe=$(cafe) --two_stage=$(ts)
endef

# Expand rules for all possible combinations
$(foreach ptfe,TRUE FALSE, \
$(foreach rtfe,TRUE FALSE, \
$(foreach porch,TRUE FALSE, \
$(foreach cafe,TRUE FALSE, \
$(foreach ts,TRUE FALSE, \
	$(eval $(GEN_RULE)) \
)))))

# ─────────────────────────────────────────────────────────────────────────────
# Convenient spec targets
# ─────────────────────────────────────────────────────────────────────────────

# All specifications (many combinations, two-stage only)
all-specs:
	$(MAKE) PERMIT_TYPE_FE="TRUE FALSE" REVIEW_TYPE_FE="TRUE FALSE" INCLUDE_PORCH="TRUE FALSE" CA_FE="TRUE FALSE" TWO_STAGE=TRUE

# Key recommended spec (two-stage with CA FE)
best-spec:
	$(MAKE) PERMIT_TYPE_FE=FALSE REVIEW_TYPE_FE=TRUE INCLUDE_PORCH=TRUE CA_FE=TRUE TWO_STAGE=TRUE

# ─────────────────────────────────────────────────────────────────────────────
# Input Symlink
# ─────────────────────────────────────────────────────────────────────────────

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf $< $@

link-inputs: ../input/permits_for_uncertainty_index.csv

# ─────────────────────────────────────────────────────────────────────────────
# Includes
# ─────────────────────────────────────────────────────────────────────────────

include ../../generic.make
