SHELL := bash
include ../../shell_functions.make

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
INPUT_FILE := ../input/rent_with_ward_distances.parquet
SCRIPT_FILE := spatial_rd_rental_disaggregate.R

# Params
YVARS := rent_price
LOGS  := TRUE
BWS   := 250
KERNELS := triangular

# --- Prefix helper ---
LOG_PREFIX_TRUE  := log_
LOG_PREFIX_FALSE :=

# -----------------------------------------------------------------------------
# Targets
# -----------------------------------------------------------------------------
# Generate sentinel file targets - each represents a completed analysis run
ALL_ANALYSES := $(foreach yvar,$(YVARS), \
                $(foreach log_stat,$(LOGS), \
                $(foreach bw,$(BWS), \
                $(foreach kernel,$(KERNELS), \
                    ../output/.disaggregated_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).SUCCESS \
                ))))

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(ALL_ANALYSES)

# -----------------------------------------------------------------------------
# Rules
# -----------------------------------------------------------------------------
# Rule Generation Template
define GEN_ANALYSIS_RULES
../output/.disaggregated_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).SUCCESS: $(SCRIPT_FILE) $(INPUT_FILE) | ../output
	@echo "→ Running disaggregated rental analysis for: $(LOG_PREFIX_$(log_stat))$(yvar) | bw=$(bw) | kernel=$(kernel)"
	Rscript $(SCRIPT_FILE) $(yvar) $(log_stat) $(bw) $(kernel)
	@touch $$@

endef

# Generate all the specific rules using eval
$(foreach yvar,$(YVARS), \
$(foreach log_stat,$(LOGS), \
$(foreach bw,$(BWS), \
$(foreach kernel,$(KERNELS), \
    $(eval $(GEN_ANALYSIS_RULES)) \
))))

# Link Inputs
link-inputs: $(INPUT_FILE)

$(INPUT_FILE): | ../input
	@ln -sf ../../calculate_rent_distances/output/rent_with_ward_distances.parquet $@

../input:
	@mkdir -p $@

../output:
	@mkdir -p $@

include ../../generic.make
