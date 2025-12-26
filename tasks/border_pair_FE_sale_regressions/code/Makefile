SHELL := bash
include ../../shell_functions.make

# --- Adjustable lists ---
# Bandwidths in FEET (standard cutoffs)
BWS_FT := 250 500 750 1000 1320 2000 2640

# Outcomes shown as columns in each table (logs allowed too)
YVARS_TABLE := 'log(sale_price)'

# --- Key Files ---
INPUT_SALES := ../input/sales_with_ward_distances.csv
SCRIPT_FILE := border_pair_FE_sale_regressions.R

# --- Outputs (one per bandwidth) ---
OUTPUT_FILES := $(foreach bw,$(BWS_FT),../output/fe_table_bw$(bw).tex)

.PHONY: all link-inputs
all: $(OUTPUT_FILES)

# Generate explicit rules for each bandwidth
define FE_template
../output/fe_table_bw$(1).tex: $(SCRIPT_FILE) $(INPUT_SALES) | ../output
	@echo "→ [FE Sale Tables] Generating table at bandwidth: $(1) ft with outcomes: $(YVARS_TABLE)"
	Rscript $(SCRIPT_FILE) "$(1)" "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(eval $(call FE_template,$(bw))))


# --- Link input from upstream task (symlink), ensure ../input exists first ---
$(INPUT_SALES): ../../calculate_sale_distances/output/sales_with_ward_distances.csv | ../input
	@echo "→ [FE Sale Tables] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

# Convenience
link-inputs: $(INPUT_SALES)

# Generic helpers
include ../../generic.make
