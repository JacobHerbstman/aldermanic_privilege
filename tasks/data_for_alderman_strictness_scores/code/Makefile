SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Define key files for this task
# ─────────────────────────────────────────────────────────────────────────────

OUTPUT_FILE_PERMITS := ../output/alderman_restrictiveness_scores_data.csv
OUTPUT_FILE_WARD    := ../output/ward_monthly_panel_for_alderman_fe.csv

SCRIPT_FILE := create_restrictiveness_score_data.R

INPUT_WARD_PANEL      := ../input/ward_panel.gpkg
INPUT_PERMITS         := ../input/building_permits_clean.gpkg
INPUT_ALDERMEN        := ../input/chicago_alderman_panel.csv
INPUT_CONTROLS        := ../input/ward_controls.csv
INPUT_CTA             := ../input/cta_stations.geojson
INPUT_BOUNDARY        := ../input/city_boundary.geojson
INPUT_COMMUNITY_AREAS := ../input/community_areas.geojson

# Lakefront (OSM water areas) — use wildcard to link all sidecars
LAKE_SRC_DIR := ../../../data_raw/illinois-250919-free
LAKE_PREFIX  := gis_osm_water_a_free_1
LAKE_SET     := $(wildcard $(LAKE_SRC_DIR)/$(LAKE_PREFIX).*)

# after linking, this file will exist in ../input/
INPUT_LAKE := ../input/$(LAKE_PREFIX).shp

# ─────────────────────────────────────────────────────────────────────────────
# Define Goals
# ─────────────────────────────────────────────────────────────────────────────

all: $(OUTPUT_FILE_WARD) $(OUTPUT_FILE_PERMITS)

# ─────────────────────────────────────────────────────────────────────────────
# Main rule to create the final output
# ─────────────────────────────────────────────────────────────────────────────
$(OUTPUT_FILE_WARD) $(OUTPUT_FILE_PERMITS): $(SCRIPT_FILE) \
	$(INPUT_WARD_PANEL) $(INPUT_PERMITS) $(INPUT_ALDERMEN) $(INPUT_CONTROLS) \
	$(INPUT_CTA) $(INPUT_BOUNDARY) $(INPUT_COMMUNITY_AREAS) $(INPUT_LAKE) | ../output
	@echo "→ [create_alderman_score] Running R script to create alderman restrictiveness scores"
	$(R) $<

# ─────────────────────────────────────────────────────────────────────────────
# Rules to create the input symbolic links
# ─────────────────────────────────────────────────────────────────────────────

$(INPUT_WARD_PANEL): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@echo "→ [create_restrictiveness_score] Linking: $@ ← $<"
	ln -sf $< $@

$(INPUT_PERMITS): ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@echo "→ [create_restrictiveness_score] Linking: $@ ← $<"
	ln -sf $< $@

$(INPUT_ALDERMEN): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@echo "→ [create_restrictiveness_score] Linking: $@ ← $<"
	ln -sf $< $@

$(INPUT_CONTROLS): ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	@echo "→ [create_restrictiveness_score] Linking: $@ ← $<"
	ln -sf $< $@

$(INPUT_CTA): ../../../data_raw/cta_stations.geojson | ../input
	@echo "→ [create_restrictiveness_score] Linking: $@ ← $<"
	ln -sf $< $@

$(INPUT_BOUNDARY): ../../../data_raw/Boundaries_-_City_20250920.geojson | ../input
	@echo "→ [create_restrictiveness_score] Linking: $@ ← $<"
	ln -sf $< $@

$(INPUT_COMMUNITY_AREAS): ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	@echo "→ [create_restrictiveness_score] Linking: $@ ← $<"
	ln -sf $< $@

# Link ALL lakefront shapefile sidecars via wildcard
$(INPUT_LAKE): $(LAKE_SET) | ../input
	@echo "→ [create_restrictiveness_score] Linking lakefront files (OSM water areas)…"
	@for f in $(LAKE_SET); do ln -sf "$$f" "../input/$$(basename $$f)"; done

# ─────────────────────────────────────────────────────────────────────────────
# Generic rules
# ─────────────────────────────────────────────────────────────────────────────
include ../../generic.make

# ─────────────────────────────────────────────────────────────────────────────
# Utility Targets
# ─────────────────────────────────────────────────────────────────────────────
.PHONY: link-inputs
link-inputs: $(INPUT_WARD_PANEL) $(INPUT_PERMITS) $(INPUT_ALDERMEN) $(INPUT_CONTROLS) \
             $(INPUT_CTA) $(INPUT_BOUNDARY) $(INPUT_COMMUNITY_AREAS) $(INPUT_LAKE)
