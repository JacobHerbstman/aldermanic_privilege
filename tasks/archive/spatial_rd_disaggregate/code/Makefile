SHELL := bash
include ../../shell_functions.make

# --- Analysis Parameters (Restored to full list) ---
BANDWIDTHS := 528 1056 1584 2640
KERNELS    := epanechnikov
YVARS      := density_far density_lapu density_bcr density_lps
LOG_STATUS := TRUE

# --- Prefix helper ---
LOG_PREFIX_TRUE  := log_
LOG_PREFIX_FALSE :=

# --- Key Files & Directories ---
INPUT_PARCELS   := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE     := spatial_rd_disaggregate.R

# --- Generate all hidden sentinel file targets in the main output directory ---
# Each sentinel file represents a completed analysis run. The prefix ".disaggregated_" prevents name collisions.
ALL_ANALYSES := $(foreach yvar,$(YVARS), \
                $(foreach bw,$(BANDWIDTHS), \
                $(foreach kernel,$(KERNELS), \
                $(foreach log_stat,$(LOG_STATUS), \
                    ../output/.disaggregated_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).SUCCESS \
                ))))

# --- Goals ---
.PHONY: all link-inputs
all: $(ALL_ANALYSES)

# --- Rule Generation Template ---
# Defines the rule for creating one batch of plots and its corresponding sentinel file.
define GEN_ANALYSIS_RULES
# Target is the hidden .SUCCESS file in the ../output directory.
../output/.disaggregated_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).SUCCESS: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ Running disaggregated analysis for: $(LOG_PREFIX_$(log_stat))$(yvar) | bw=$(bw) | kernel=$(kernel)"
	Rscript $(SCRIPT_FILE) $(yvar) $(log_stat) $(bw) $(kernel)
	# If the script completes, create the sentinel file to mark it as done.
	@touch $$@

endef

# --- Generate all the specific rules using eval ---
$(foreach yvar,$(YVARS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach kernel,$(KERNELS), \
$(foreach log_stat,$(LOG_STATUS), \
    $(eval $(GEN_ANALYSIS_RULES)) \
))))

$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@ln -sf $< $@

# --- Input Linking ---
# Updated 'clean' rule to remove the generated plots and their sentinel files from ../output
link-inputs: $(INPUT_PARCELS)
include ../../generic.make