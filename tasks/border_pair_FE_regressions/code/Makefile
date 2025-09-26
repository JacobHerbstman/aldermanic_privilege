SHELL := bash
include ../../shell_functions.make

# --- Adjustable lists ---
# Bandwidths in FEET
BWS_FT := 528 792 1056 1320 1584

# Outcomes shown as columns in each table (logs allowed too)
# Pass these to R as space-separated args (no commas).
YVARS_TABLE := density_far density_lapu areabuilding unitscount

# --- Key Files ---
INPUT_PARCELS := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE   := border_pair_FE_regressions.R

# --- Outputs (one per bandwidth) ---
OUTPUT_FILES := $(foreach bw,$(BWS_FT),../output/fe_table_bw$(bw).tex)

.PHONY: all link-inputs
all: $(OUTPUT_FILES)

# Generate explicit rules for each bandwidth (avoids brittle pattern pitfalls)
define FE_template
../output/fe_table_bw$(1).tex: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ [FE Tables] Generating table at bandwidth: $(1) ft with outcomes: $(YVARS_TABLE)"
	Rscript $(SCRIPT_FILE) "$(1)" "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(eval $(call FE_template,$(bw))))


# --- Link input from upstream task (symlink), ensure ../input exists first ---
$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@echo "→ [FE Tables] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

# Convenience
link-inputs: $(INPUT_PARCELS)

# Generic helpers (ok to keep; our explicit dir rules above already cover mkdir)
include ../../generic.make
