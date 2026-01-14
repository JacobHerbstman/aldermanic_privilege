SHELL := bash
include ../../shell_functions.make

# --- Placebo Shifts (in feet) ---
# Test cutoffs shifted left (-) and right (+) from the true boundary
PLACEBO_SHIFTS := -500 -250 250 500

# --- Fixed bandwidth for placebo tests ---
BW := 50

# --- Key Files ---
INPUT_FILE := ../input/rent_with_ward_distances.parquet
SCRIPT_FILE := spatial_rd_rental_placebo_boundaries.R

# --- Outputs (one per placebo shift) ---
# Generate output file names: placebo_rd_shift_plus250_bw250.pdf, etc.
define shift_to_filename
$(if $(filter -%,$(1)),minus$(subst -,,$(1)),plus$(1))
endef

OUTPUT_PDFS := $(foreach s,$(PLACEBO_SHIFTS),../output/placebo_rd_shift_$(call shift_to_filename,$(s))_bw$(BW).pdf)
OUTPUT_CSVS := $(foreach s,$(PLACEBO_SHIFTS),../output/placebo_results_shift_$(call shift_to_filename,$(s))_bw$(BW).csv)

.PHONY: all link-inputs clean
all: $(OUTPUT_PDFS)

# Generate explicit rules for each placebo shift
define PLACEBO_template
../output/placebo_rd_shift_$(call shift_to_filename,$(1))_bw$(BW).pdf: $(SCRIPT_FILE) $(INPUT_FILE) | ../output
	@echo "→ [Placebo RD] Running placebo test with shift = $(1) ft"
	Rscript $(SCRIPT_FILE) "$(1)"
endef
$(foreach s,$(PLACEBO_SHIFTS),$(eval $(call PLACEBO_template,$(s))))

# --- Link input from upstream task ---
$(INPUT_FILE): ../../calculate_rent_distances/output/rent_with_ward_distances.parquet | ../input
	@echo "→ [Placebo RD] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

link-inputs: $(INPUT_FILE)

# Combine all CSVs into one summary file
../output/placebo_summary.csv: $(OUTPUT_CSVS)
	@echo "→ [Placebo RD] Combining results into summary..."
	head -1 $(word 1,$(OUTPUT_CSVS)) > $@
	tail -q -n +2 $(OUTPUT_CSVS) >> $@
	@echo "✓ Summary saved to: $@"

summary: ../output/placebo_summary.csv

clean:
	rm -f ../output/*.pdf ../output/*.csv

include ../../generic.make
