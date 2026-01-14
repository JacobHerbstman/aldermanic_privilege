SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# --- Main Goal: Create the final map image ---
all: ../output/land_share_by_ward_2020.pdf.pdf

# --- Rule to generate the map from the R script ---
../output/construction_map_2006_2014_w_density.pdf: land_values_maps.R ../input/land_event_study_data.csv ../input/ward_panel.gpkg | ../output
	@echo "→ Generating construction map..."
	Rscript land_values_maps.R

# --- Rules for linking input files from other tasks ---
../input/land_event_study_data.csv: ../../land_value_event_study_data/output/land_event_study_data.csv | ../input
	@ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@ln -sf $< $@

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	@echo "→ [merge_wards_blocks] Linking raw input: census_blocks_2010.csv"
	ln -sf ../../../data_raw/CensusBlockTIGER2010_20250721.csv $@


# --- Helper to create all input links at once ---
link-inputs: ../input/land_event_study_data.csv ../input/ward_panel.gpkg ../input/census_blocks_2010.csv

# --- Include generic rules for directory creation etc. ---
include ../../generic.make