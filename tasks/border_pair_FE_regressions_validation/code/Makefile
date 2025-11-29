SHELL := bash
include ../../shell_functions.make

# --- Adjustable lists ---
# Bandwidths in FEET
BWS_FT := 100 150 200 250 264 500 528 750 792 1000 1056 1320

# --- Key Files ---
INPUT_PARCELS := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE   := border_pair_FE_regressions_validation.R

# --- Outputs (one per bandwidth) ---
OUTPUT_FILES := $(foreach bw,$(BWS_FT),../output/fe_validation_table_bw$(bw).tex)

.PHONY: all link-inputs

# Default target runs only the validation regressions
all: $(OUTPUT_FILES)

# --- Rule Definition ---
# Calls the R script with arguments: <bandwidth> <output_file>
define VAL_template
../output/fe_validation_table_bw$(1).tex: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ [Validation Checks] Generating balance table at bandwidth: $(1) ft"
	Rscript $(SCRIPT_FILE) "$(1)" "$$@"
endef
$(foreach bw,$(BWS_FT),$(eval $(call VAL_template,$(bw))))


# --- Link input from upstream task ---
$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@echo "→ [Validation Checks] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

# Convenience target to set up inputs
link-inputs: $(INPUT_PARCELS)


# Generic helpers
include ../../generic.make