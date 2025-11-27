SHELL := bash
include ../../shell_functions.make

# --- Adjustable lists ---
BWS_FT := 100 150 200 250 264 500 528 750 792 1000 1056 1320
YVARS_TABLE := dist_cta_stop dist_park dist_school dist_major_street

# --- Key files ---
# --- Key files ---
SCRIPT_FILE := border_pair_FE_placebos.R
PREP_SCRIPT := prepare_placebo_data.R
INPUT_PARCELS := ../input/parcels_with_ward_distances.csv
INPUT_GEO := ../input/parcels_with_geometry.gpkg
INPUT_AMENITIES := ../input/cta_stops.gpkg ../input/parks.gpkg ../input/schools_2015.gpkg ../input/major_streets.gpkg
DATA_PLACEBOS := ../output/parcels_with_placebos.csv

# --- Outputs: one per bandwidth ---
OUTPUT_FILES := $(foreach bw,$(BWS_FT),../output/placebo_bw$(bw).tex)

.PHONY: all link-inputs
all: $(OUTPUT_FILES)

# Rule to prepare data
$(DATA_PLACEBOS): $(PREP_SCRIPT) $(INPUT_PARCELS) $(INPUT_GEO) $(INPUT_AMENITIES) | ../output
	@echo "→ [Placebos] Preparing placebo data (calculating distances)..."
	Rscript $(PREP_SCRIPT)

# Explicit rules per bandwidth (space-separated yvars)
define PL_template
../output/placebo_bw$(1).tex: $(SCRIPT_FILE) $(DATA_PLACEBOS) | ../output
	@echo "→ [Placebos] Generating placebo table at bandwidth: $(1) ft with outcomes: $(YVARS_TABLE)"
	Rscript $(SCRIPT_FILE) "$(1)" "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(eval $(call PL_template,$(bw))))

# --- Symlink input from upstream task ---
$(INPUT_PARCELS): ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	@echo "→ [Placebos] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

$(INPUT_GEO): ../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@echo "→ [Placebos] Linking input: $@ ← $<"
	ln -sf "$<" "$@"

../input/cta_stops.gpkg: ../../download_amenities_data/output/cta_stops.gpkg | ../input
	ln -sf "$<" "$@"
../input/parks.gpkg: ../../download_amenities_data/output/parks.gpkg | ../input
	ln -sf "$<" "$@"
../input/schools_2015.gpkg: ../../download_amenities_data/output/schools_2015.gpkg | ../input
	ln -sf "$<" "$@"
../input/major_streets.gpkg: ../../download_amenities_data/output/major_streets.gpkg | ../input
	ln -sf "$<" "$@"

# Convenience
link-inputs: $(INPUT_PARCELS) $(INPUT_GEO) $(INPUT_AMENITIES)

include ../../generic.make
