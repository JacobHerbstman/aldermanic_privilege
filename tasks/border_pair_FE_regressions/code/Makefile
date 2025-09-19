# border_pair_FE_regressions/Makefile

SHELL := bash
include ../../shell_functions.make

# --- Analysis Parameters ---
YVARS := density_far log(density_far) density_lapu log(density_lapu) density_bcr log(density_bcr) density_lps log(density_lps) density_spu log(density_spu) arealotsf log(arealotsf) storiescount log(storiescount) unitscount log(unitscount)

# --- Helper function to create clean filenames from yvar strings ---
# This function converts variable names to valid filenames
get_stem = $(shell echo "$(1)" | sed 's/log(/log_/g' | sed 's/)//g')

# --- Generate Output Filenames ---
OUTPUT_FILES := $(foreach yvar,$(YVARS),../output/fe_table_$(call get_stem,$(yvar)).tex)

# --- Key Files ---
INPUT_PARCELS := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE   := border_pair_FE_regressions.R

# --- Goals ---
.PHONY: all link-inputs
all: $(OUTPUT_FILES)

# --- Dynamic Rule Generation Template ---
define GEN_RULE
../output/fe_table_$(call get_stem,$(1)).tex: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ [FE Regressions] Generating table for outcome: $(1)"
	Rscript $(SCRIPT_FILE) "$(1)" $$@
endef

# --- Instantiate a rule for each YVAR ---
$(foreach yvar,$(YVARS),$(eval $(call GEN_RULE,$(yvar))))

# --- Input Linking ---
$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@echo "→ [FE Regressions] Linking input: $@ ← $<"
	ln -sf $< $@

# --- Generic Rules & Utilities ---
include ../../generic.make
link-inputs: $(INPUT_PARCELS)