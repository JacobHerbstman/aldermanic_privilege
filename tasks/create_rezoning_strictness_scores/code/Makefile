SHELL := bash
include ../../shell_functions.make

SCRIPT := create_rezoning_strictness_score.R

OUTPUT_CSV := ../output/alderman_rezoning_strictness_scores.csv
OUTPUT_PLOT := ../output/rezoning_strictness_index.pdf
USE_ALL_REZONINGS ?= FALSE

INPUT_REZONING := ../input/rezoning_dataset_usable_19990101_20260212.csv
INPUT_BG_CONTROLS := ../input/block_group_controls_2000_2023.csv
INPUT_ALDERMAN_PANEL := ../input/chicago_alderman_panel.csv
INPUT_CTA := ../input/cta_stations.geojson
INPUT_CITY := ../input/city_boundary.geojson
INPUT_WATER := ../input/gis_osm_water_a_free_1.shp

all: $(OUTPUT_CSV) $(OUTPUT_PLOT)

$(OUTPUT_CSV) $(OUTPUT_PLOT): $(SCRIPT) $(INPUT_REZONING) $(INPUT_BG_CONTROLS) $(INPUT_ALDERMAN_PANEL) $(INPUT_CTA) $(INPUT_CITY) $(INPUT_WATER) | ../output
	$(R) $(SCRIPT) --input_rezoning=$(INPUT_REZONING) --input_bg_controls=$(INPUT_BG_CONTROLS) --input_alderman_panel=$(INPUT_ALDERMAN_PANEL) --input_cta=$(INPUT_CTA) --input_city=$(INPUT_CITY) --input_water=$(INPUT_WATER) --output_csv=$(OUTPUT_CSV) --output_plot=$(OUTPUT_PLOT) --use_all_rezonings=$(USE_ALL_REZONINGS)

$(INPUT_REZONING): /Users/jacobherbstman/Desktop/alderman_data/tasks/rezoning_dataset/output/rezoning_dataset_usable_19990101_20260212.csv | ../input
	ln -sf "$<" "$@"

$(INPUT_BG_CONTROLS): ../../create_ward_controls/output/block_group_controls_2000_2023.csv | ../input
	ln -sf "$<" "$@"

$(INPUT_ALDERMAN_PANEL): ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

$(INPUT_CTA): ../../../data_raw/cta_stations.geojson | ../input
	ln -sf "$<" "$@"

$(INPUT_CITY): ../../../data_raw/Boundaries_-_City_20250920.geojson | ../input
	ln -sf "$<" "$@"

$(INPUT_WATER): ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	ln -sf ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.* $(dir $@)

include ../../generic.make

link-inputs: $(INPUT_REZONING) $(INPUT_BG_CONTROLS) $(INPUT_ALDERMAN_PANEL) $(INPUT_CTA) $(INPUT_CITY) $(INPUT_WATER)
