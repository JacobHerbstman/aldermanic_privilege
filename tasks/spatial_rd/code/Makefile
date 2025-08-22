SHELL := bash
include ../../shell_functions.make

# --- Analysis Parameters ---
BANDWIDTHS := 50 100 150 200 250 300 400 500
KERNELS    := triangular epanechnikov uniform
YVARS      := density_far

# --- Key Files ---
INPUT_PARCELS := ../input/parcels_with_ward_distances.gpkg
SCRIPT_FILE   := spatial_rd.R

# --- Generate all output filenames for both plot types ---
ALL_RD_PLOTS      = $(foreach yvar,$(YVARS), $(foreach bw,$(BANDWIDTHS), $(foreach kernel,$(KERNELS), ../output/rd_plot_$(yvar)_bw$(bw)_$(kernel).png)))
ALL_SCATTER_PLOTS = $(foreach yvar,$(YVARS), $(foreach bw,$(BANDWIDTHS), $(foreach kernel,$(KERNELS), ../output/rd_scatter_$(yvar)_bw$(bw)_$(kernel).png)))

# --- Goals ---
.PHONY: all link-inputs
all: $(ALL_RD_PLOTS) $(ALL_SCATTER_PLOTS)

# --- Rule Generation Template ---
define GEN_PLOTTING_RULES
# Primary rule
../output/rd_plot_$(yvar)_bw$(bw)_$(kernel).png: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ Generating plots for: $(yvar) | bw=$(bw) | kernel=$(kernel)"
	Rscript $(SCRIPT_FILE) $(yvar) $(bw) $(kernel) ../output/rd_plot_$(yvar)_bw$(bw)_$(kernel).png ../output/rd_scatter_$(yvar)_bw$(bw)_$(kernel).png

# Side-effect rule
../output/rd_scatter_$(yvar)_bw$(bw)_$(kernel).png: ../output/rd_plot_$(yvar)_bw$(bw)_$(kernel).png
	@# This file is generated alongside the rd_plot.

endef

# --- Generate all the specific rules using eval ---
$(foreach yvar,$(YVARS), \
    $(foreach bw,$(BANDWIDTHS), \
        $(foreach kernel,$(KERNELS), \
            $(eval $(GEN_PLOTTING_RULES)) \
        ) \
    ) \
)

# --- Input Linking & Generic Rules ---
include ../../generic.make