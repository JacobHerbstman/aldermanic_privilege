SHELL := bash
include ../../shell_functions.make

# --- Key Files ---
INPUT_PARCELS := ../input/parcels_with_ward_distances.csv
SCRIPT_FILE   := summary_stats.R
OUTPUT_FILE   := ../output/summary_stats.tex

.PHONY: all link-inputs

all: $(OUTPUT_FILE)

$(OUTPUT_FILE): $(SCRIPT_FILE) $(INPUT_PARCELS) | ../output
	@echo "→ [Summary Stats] Generating table..."
	Rscript $(SCRIPT_FILE) $(INPUT_PARCELS) $(OUTPUT_FILE)

# --- Link input from upstream task ---
$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@echo "→ [Summary Stats] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

link-inputs: $(INPUT_PARCELS)

include ../../generic.make
