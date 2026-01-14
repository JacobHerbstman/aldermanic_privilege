SHELL := bash
include ../../shell_functions.make

# --- Adjustable lists ---
# Bandwidths in FEET - matching the original analysis
BWS_FT := 250 500 1000

YVARS_TABLE := 'log(density_far)' 'log(density_dupac)' 'log(unitscount)'

# --- Key Files ---
INPUT_PARCELS := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE   := border_pair_FE_regressions_linear_trends.R

# --- Outputs (one per bandwidth) ---
OUTPUT_FILES := $(foreach bw,$(BWS_FT),../output/fe_linear_trends_bw$(bw).tex)

.PHONY: all link-inputs clean
all: $(OUTPUT_FILES)

# Generate explicit rules for each bandwidth
define FE_template
../output/fe_linear_trends_bw$(1).tex: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ [Linear Trends] Generating table at bandwidth: $(1) ft"
	Rscript $(SCRIPT_FILE) "$(1)" "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(eval $(call FE_template,$(bw))))

# --- Link input from upstream task ---
$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@echo "→ [Linear Trends] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

link-inputs: $(INPUT_PARCELS)

clean:
	rm -f ../output/*.tex ../output/*.csv

# Generic helpers
include ../../generic.make
