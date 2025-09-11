SHELL := bash
include ../../shell_functions.make

INPUT_SHAPEFILE := ../input/raw_zoning_data.geojson
INPUT_ZONING := ../input/zoning-code-summary-district-types.csv
SCRIPT := clean_zoning_data.R
OUTPUT_FILE := ../output/zoning_data_clean.gpkg

.DEFAULT_GOAL := $(OUTPUT_FILE)
.PHONY: all
all: $(OUTPUT_FILE)

# 2) Run the cleaner to produce the GPKG
$(OUTPUT_FILE): clean_zoning_data.R $(INPUT_SHAPEFILE) $(INPUT_ZONING) | ../output
	@echo "→ [clean_zoning_data] Running R script to clean zoning data..."
	$(R) $<

$(INPUT_SHAPEFILE): ../../../data_raw/Boundaries_-_Zoning_Districts_20250910.geojson | ../input
	@echo "→ [clean_zoning_data] Linking raw data input..."
	ln -s $< $@

$(INPUT_ZONING): ../../../data_raw/zoning-code-summary-district-types.csv | ../input
	@echo "→ [clean_zoning_data] Linking raw data input..."
	ln -s $< $@

link-inputs: $(INPUT_SHAPEFILE) $(INPUT_ZONING)

include ../../generic.make
