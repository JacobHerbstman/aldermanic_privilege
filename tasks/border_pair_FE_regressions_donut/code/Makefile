SHELL := bash
include ../../shell_functions.make

# --- Adjustable lists ---
# Bandwidths in FEET
BWS_FT := 250

# Donut sizes in FEET
DONUTS_FT := 25 50

YVARS_TABLE := 'log(density_far)' 'log(density_dupac)' 'log(unitscount)'

# --- Key Files ---
INPUT_PARCELS := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE   := border_pair_FE_regressions_donut.R

# --- Outputs (one per bandwidth × donut) ---
define GEN_OUTPUTS
$(foreach bw,$(BWS_FT),$(foreach donut,$(DONUTS_FT),../output/fe_table_bw$(bw)_donut$(donut).tex ))
endef
OUTPUT_FILES := $(call GEN_OUTPUTS)

.PHONY: all link-inputs
all: $(OUTPUT_FILES)

# Generate explicit rules for each bandwidth × donut combination
define FE_template
../output/fe_table_bw$(1)_donut$(2).tex: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ [FE Donut] bw=$(1)ft donut=$(2)ft outcomes: $(YVARS_TABLE)"
	Rscript $(SCRIPT_FILE) "$(1)" "$(2)" "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(foreach donut,$(DONUTS_FT),$(eval $(call FE_template,$(bw),$(donut)))))


# --- Link input from upstream task ---
$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@echo "→ [FE Donut] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

# Convenience
link-inputs: $(INPUT_PARCELS)

# Generic helpers
include ../../generic.make
