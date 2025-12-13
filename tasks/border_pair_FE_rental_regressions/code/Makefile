SHELL := bash
include ../../shell_functions.make

# --- Adjustable lists ---
# Bandwidths in FEET (matching spatial_rd_rental bandwidths)
BWS_FT := 50 100 200 250 500 750 1000 1320

# Outcomes shown as columns in each table (logs allowed too)
YVARS_TABLE := 'log(rent_price)'

# --- Key Files ---
INPUT_PARCELS := ../input/rent_with_ward_distances.parquet
SCRIPT_FILE   := border_pair_FE_rental_regressions.R

# --- Outputs (one per bandwidth) ---
OUTPUT_FILES := $(foreach bw,$(BWS_FT),../output/fe_table_bw$(bw).tex)

.PHONY: all link-inputs
all: $(OUTPUT_FILES)

# Generate explicit rules for each bandwidth
define FE_template
../output/fe_table_bw$(1).tex: $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ [FE Rental Tables] Generating table at bandwidth: $(1) ft with outcomes: $(YVARS_TABLE)"
	Rscript $(SCRIPT_FILE) "$(1)" "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(eval $(call FE_template,$(bw))))


# --- Link input from upstream task (symlink), ensure ../input exists first ---
$(INPUT_PARCELS): ../../calculate_rent_distances/output/rent_with_ward_distances.parquet | ../input
	@echo "→ [FE Rental Tables] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

# Convenience
link-inputs: $(INPUT_PARCELS)

# Generic helpers
include ../../generic.make
