SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────
# The R script that does the work.
SCRIPT_FILE := make_ward_panel.R

# Define the OUTPUT
OUTPUT_FILE := ../output/ward_panel.gpkg

# Define the INPUTS. These will all be created as symbolic links.
INPUT_WARD_1998  := ../input/ward1998.shp
INPUT_WARD_2014  := ../input/Wards_2014.geojson
INPUT_WARD_2015  := ../input/Wards_2015.geojson
INPUT_WARD_2024  := ../input/Wards_2024.geojson

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

.DEFAULT_GOAL := all
.PHONY: all
all: $(OUTPUT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# This rule depends on the script and both input ward files.
# ─────────────────────────────────────────────────────────────────────────────
$(OUTPUT_FILE): $(SCRIPT_FILE) $(INPUT_WARD_1998) $(INPUT_WARD_2014) $(INPUT_WARD_2015) $(INPUT_WARD_2024) | ../output
	@echo "→ [make_ward_panel] Running R script to create ward panel..."
	$(R) $(SCRIPT_FILE)

# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# ─────────────────────────────────────────────────────────────────────────────

$(INPUT_WARD_1998): ../../../data_raw/ward1998.shp | ../input
	@echo "→ [make_ward_panel] Linking raw input: ward1998.*"
	ln -sf ../../../data_raw/ward1998.* $(dir $@)

# Rule for linking the 2014 ward data from the 'data_raw' directory.
$(INPUT_WARD_2014): ../../../data_raw/Wards_2014.geojson | ../input
	@echo "→ [make_ward_panel] Linking raw input: Wards_2014.geojson"
	ln -sf ../../../data_raw/Wards_2014.geojson $@

# Rule for linking the 2015 ward data from the 'data_raw' directory.
$(INPUT_WARD_2015): ../../../data_raw/Wards_2015.geojson | ../input
	@echo "→ [make_ward_panel] Linking raw input: Wards_2015.geojson"
	ln -sf ../../../data_raw/Wards_2015.geojson $@

# Rule for linking the 2015 ward data from the 'data_raw' directory.
$(INPUT_WARD_2024): ../../../data_raw/Wards_2024.geojson | ../input
	@echo "→ [make_ward_panel] Linking raw input: Wards_2024.geojson"
	ln -sf ../../../data_raw/Wards_2024.geojson $@

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────

.PHONY: link-inputs
link-inputs: $(INPUT_WARD_1998) $(INPUT_WARD_2014) $(INPUT_WARD_2015) $(INPUT_WARD_2024)