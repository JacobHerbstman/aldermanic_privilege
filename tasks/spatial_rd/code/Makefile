SHELL := bash
include ../../shell_functions.make

# --- Analysis Parameters ---
BANDWIDTHS := 264 528 792 1056 1320 1584 2112 2640
KERNELS    := triangular epanechnikov uniform
YVARS      := density_far density_lapu density_bcr density_lps density_spu
LOG_STATUS := TRUE FALSE

# --- Prefix helper for filenames ---
LOG_PREFIX_TRUE  := log_
LOG_PREFIX_FALSE :=

# --- Key Files ---
INPUT_PARCELS := ../input/parcels_with_ward_distances.gpkg
SCRIPT_FILE   := spatial_rd.R

# --- Generate all output filenames for both plot types and log statuses ---
define GEN_FILENAMES
    $(foreach yvar,$(YVARS), \
    $(foreach bw,$(BANDWIDTHS), \
    $(foreach kernel,$(KERNELS), \
    $(foreach log_stat,$(LOG_STATUS), \
        ../output/$(1)_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).png \
    ))))
endef

ALL_RD_PLOTS      := $(call GEN_FILENAMES,rd_plot)
ALL_SCATTER_PLOTS := $(call GEN_FILENAMES,rd_scatter)

# --- Goals ---
.PHONY: all link-inputs
all: $(ALL_RD_PLOTS) $(ALL_SCATTER_PLOTS)

# --- Rule Generation Template ---
define GEN_PLOTTING_RULES
# Primary rule for a specific combination of parameters
../output/rd_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).png: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ Generating plots for: $(LOG_PREFIX_$(log_stat))$(yvar) | bw=$(bw) | kernel=$(kernel)"
	Rscript $(SCRIPT_FILE) $(yvar) $(log_stat) $(bw) $(kernel) ../output/rd_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).png ../output/rd_scatter_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).png

# Side-effect rule: marks the scatter plot as created when the rd_plot is made
../output/rd_scatter_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).png: ../output/rd_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).png
	@# This file is generated alongside the rd_plot.

endef

# --- Generate all the specific rules using eval ---
$(foreach yvar,$(YVARS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach kernel,$(KERNELS), \
$(foreach log_stat,$(LOG_STATUS), \
    $(eval $(GEN_PLOTTING_RULES)) \
))))

# --- Input Linking & Generic Rules ---
include ../../generic.make