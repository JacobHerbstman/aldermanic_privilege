SHELL := bash
include ../../shell_functions.make

# --- Analysis Parameters ---
BANDWIDTHS := 200 250 264 500 528 792 1056 1320
KERNELS    := triangular
YVARS      := density_far density_dupac unitscount
#density_bcr density_lps density_lapu
LOG_STATUS := TRUE FALSE
DONUT := 0

# --- Prefix helper for filenames ---
LOG_PREFIX_TRUE  := log_
LOG_PREFIX_FALSE :=

# --- Key Files ---
INPUT_PARCELS := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE   := spatial_rd_same_zone_only.R

# --- Generate all output filenames (only rd_plot now) ---
define GEN_FILENAMES
    $(foreach yvar,$(YVARS), \
    $(foreach bw,$(BANDWIDTHS), \
    $(foreach kernel,$(KERNELS), \
    $(foreach log_stat,$(LOG_STATUS), \
        ../output/rd_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).pdf \
    ))))
endef

ALL_RD_PLOTS := $(call GEN_FILENAMES)

# --- Goals ---
.PHONY: all link-inputs
all: $(ALL_RD_PLOTS)

# --- Rule Generation Template (single output & single path passed to R) ---
define GEN_PLOTTING_RULES
../output/rd_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).pdf: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ Generating plot for: $(LOG_PREFIX_$(log_stat))$(yvar) | bw=$(bw) | kernel=$(kernel)"
	Rscript $(SCRIPT_FILE) $(yvar) $(log_stat) $(bw) $(kernel) $$@
endef

# --- Generate the specific rules ---
$(foreach yvar,$(YVARS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach kernel,$(KERNELS), \
$(foreach log_stat,$(LOG_STATUS), \
    $(eval $(GEN_PLOTTING_RULES)) \
))))

$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@ln -sf $< $@

# --- Input Linking & Generic Rules ---
link-inputs: $(INPUT_PARCELS)
include ../../generic.make
