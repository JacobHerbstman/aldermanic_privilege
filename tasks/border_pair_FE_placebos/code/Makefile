SHELL := bash
include ../../shell_functions.make

# --- Adjustable lists ---
BWS_FT := 528 792 1056 1320 1584
YVARS_TABLE := dist_cta_stop dist_park dist_school dist_major_street

# --- Key files ---
SCRIPT_FILE := border_pair_FE_placebos.R
INPUT_PARCELS := ../input/parcels_with_ward_distances.csv

# --- Outputs: one per bandwidth ---
OUTPUT_FILES := $(foreach bw,$(BWS_FT),../output/placebo_bw$(bw).tex)

.PHONY: all link-inputs
all: $(OUTPUT_FILES)

# Explicit rules per bandwidth (space-separated yvars)
define PL_template
../output/placebo_bw$(1).tex: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ [Placebos] Generating placebo table at bandwidth: $(1) ft with outcomes: $(YVARS_TABLE)"
	Rscript $(SCRIPT_FILE) "$(1)" "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(eval $(call PL_template,$(bw))))

# --- Symlink input from upstream task ---
$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@echo "→ [Placebos] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

# Convenience
link-inputs: $(INPUT_PARCELS)

include ../../generic.make
