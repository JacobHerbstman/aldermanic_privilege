SHELL := bash
include ../../shell_functions.make

# --- Polynomial orders to test ---
# 1 = Linear (baseline), 2 = Quadratic, 3 = Cubic
POLY_ORDERS := 1 2 3

# --- Fixed bandwidth ---
BW := 500

# --- Key Files ---
INPUT_FILE := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE := spatial_rd_same_zone_only_function_robustness.R

# --- Polynomial order to label mapping ---
poly_label = $(if $(filter 1,$(1)),linear,$(if $(filter 2,$(1)),quadratic,cubic))

# --- Outputs ---
OUTPUT_PDFS := $(foreach p,$(POLY_ORDERS),../output/rd_same_zone_$(call poly_label,$(p))_p$(p)_bw$(BW).pdf)
OUTPUT_CSVS := $(foreach p,$(POLY_ORDERS),../output/rd_results_same_zone_$(call poly_label,$(p))_p$(p)_bw$(BW).csv)

.PHONY: all link-inputs clean summary
all: $(OUTPUT_PDFS)

# Generate explicit rules for each polynomial order
define POLY_template
../output/rd_same_zone_$(call poly_label,$(1))_p$(1)_bw$(BW).pdf: $(SCRIPT_FILE) $(INPUT_FILE) | ../output
	@echo "→ [Function Robustness] Running RD with polynomial order p = $(1)"
	Rscript $(SCRIPT_FILE) "$(1)"
endef
$(foreach p,$(POLY_ORDERS),$(eval $(call POLY_template,$(p))))

# --- Link input from upstream task ---
$(INPUT_FILE): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@echo "→ [Function Robustness] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

link-inputs: $(INPUT_FILE)

# Combine all CSVs into one summary file
../output/function_robustness_summary.csv: $(OUTPUT_CSVS)
	@echo "→ [Function Robustness] Combining results into summary..."
	head -1 $(word 1,$(OUTPUT_CSVS)) > $@
	tail -q -n +2 $(OUTPUT_CSVS) >> $@
	@echo "✓ Summary saved to: $@"

summary: ../output/function_robustness_summary.csv

clean:
	rm -f ../output/*.pdf ../output/*.csv

include ../../generic.make
