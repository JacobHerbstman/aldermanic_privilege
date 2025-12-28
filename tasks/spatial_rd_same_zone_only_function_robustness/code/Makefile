SHELL := bash
include ../../shell_functions.make

# --- Analysis Parameters ---
POLY_ORDERS := 1 2 3
BANDWIDTHS  := 500
KERNELS     := triangular
YVARS       := density_dupac density_far
LOG_STATUS  := TRUE
DONUT       := 0

# --- Prefix helper for filenames ---
LOG_PREFIX_TRUE  := log_
LOG_PREFIX_FALSE :=

# --- Key Files ---
INPUT_FILE  := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE := spatial_rd_same_zone_only_function_robustness.R

# --- Polynomial order to label mapping ---
POLY_LABEL_1 := linear
POLY_LABEL_2 := quadratic
POLY_LABEL_3 := cubic
POLY_LABEL_4 := quartic

# --- Generate all output filenames ---
define GEN_FILENAMES
    $(foreach yvar,$(YVARS), \
    $(foreach bw,$(BANDWIDTHS), \
    $(foreach kernel,$(KERNELS), \
    $(foreach log_stat,$(LOG_STATUS), \
    $(foreach p,$(POLY_ORDERS), \
        ../output/rd_$(LOG_PREFIX_$(log_stat))$(yvar)_$(POLY_LABEL_$(p))_p$(p)_bw$(bw)_$(kernel).pdf \
    )))))
endef

ALL_OUTPUTS := $(call GEN_FILENAMES)

# --- Goals ---
.PHONY: all link-inputs clean
all: $(ALL_OUTPUTS)

# --- Rule Generation Template ---
# Arguments passed to R: yvar use_log bw kernel poly_order output_file
define GEN_PLOTTING_RULES
../output/rd_$(LOG_PREFIX_$(log_stat))$(yvar)_$(POLY_LABEL_$(p))_p$(p)_bw$(bw)_$(kernel).pdf: $(SCRIPT_FILE) $(INPUT_FILE) | ../output
	@echo "→ [Function Robustness] $(yvar) (log=$(log_stat)) | bw=$(bw) | p=$(p) ($(POLY_LABEL_$(p)))"
	Rscript $(SCRIPT_FILE) $(yvar) $(log_stat) $(bw) $(kernel) $(p) $$@
endef

# --- Generate the specific rules ---
$(foreach yvar,$(YVARS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach kernel,$(KERNELS), \
$(foreach log_stat,$(LOG_STATUS), \
$(foreach p,$(POLY_ORDERS), \
    $(eval $(GEN_PLOTTING_RULES)) \
)))))

# --- Link input from upstream task ---
$(INPUT_FILE): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@echo "→ [Function Robustness] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

link-inputs: $(INPUT_FILE)

clean:
	rm -f ../output/*.pdf ../output/*.csv

include ../../generic.make
