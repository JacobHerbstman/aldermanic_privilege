SHELL := bash
include ../../shell_functions.make

# --- Analysis Parameters ---
PLACEBO_SHIFTS := -500 -250 250 500
BANDWIDTHS     := 500
KERNELS        := triangular
YVARS          := density_dupac density_far
LOG_STATUS     := TRUE

# --- Prefix helper for filenames ---
LOG_PREFIX_TRUE  := log_
LOG_PREFIX_FALSE :=

# --- Placebo shift to filename helper ---
define shift_to_filename
$(if $(filter -%,$(1)),minus$(subst -,,$(1)),plus$(1))
endef

# --- Key Files ---
INPUT_FILE  := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE := spatial_rd_same_zone_only_placebo_boundaries.R

# --- Generate all output filenames ---
define GEN_FILENAMES
    $(foreach yvar,$(YVARS), \
    $(foreach bw,$(BANDWIDTHS), \
    $(foreach kernel,$(KERNELS), \
    $(foreach log_stat,$(LOG_STATUS), \
    $(foreach s,$(PLACEBO_SHIFTS), \
        ../output/placebo_rd_$(LOG_PREFIX_$(log_stat))$(yvar)_shift_$(call shift_to_filename,$(s))_bw$(bw)_$(kernel).pdf \
    )))))
endef

ALL_OUTPUTS := $(call GEN_FILENAMES)

# --- Goals ---
.PHONY: all link-inputs clean summary
all: $(ALL_OUTPUTS)

# --- Rule Generation Template ---
# Arguments passed to R: yvar use_log bw kernel placebo_shift output_file
define GEN_PLOTTING_RULES
../output/placebo_rd_$(LOG_PREFIX_$(log_stat))$(yvar)_shift_$(call shift_to_filename,$(s))_bw$(bw)_$(kernel).pdf: $(SCRIPT_FILE) $(INPUT_FILE) | ../output
	@echo "→ [Placebo RD] $(yvar) (log=$(log_stat)) | bw=$(bw) | shift=$(s)"
	Rscript $(SCRIPT_FILE) $(yvar) $(log_stat) $(bw) $(kernel) $(s) $$@
endef

# --- Generate the specific rules ---
$(foreach yvar,$(YVARS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach kernel,$(KERNELS), \
$(foreach log_stat,$(LOG_STATUS), \
$(foreach s,$(PLACEBO_SHIFTS), \
    $(eval $(GEN_PLOTTING_RULES)) \
)))))

# --- Link input from upstream task ---
$(INPUT_FILE): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@echo "→ [Placebo RD] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

link-inputs: $(INPUT_FILE)

clean:
	rm -f ../output/*.pdf ../output/*.csv

include ../../generic.make
