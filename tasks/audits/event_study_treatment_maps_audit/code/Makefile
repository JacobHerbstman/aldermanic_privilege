SHELL := bash

MAP_BW_M ?= 304.8
MAP_BW_LABEL ?= 1000ft

all: \
	../output/treatment_control_map_2015_$(MAP_BW_LABEL)_data.csv \
	../output/treatment_control_map_2023_$(MAP_BW_LABEL)_data.csv \
	../output/treatment_control_map_2023_$(MAP_BW_LABEL).pdf \
	../output/ward_pair_before_after_13-23_$(MAP_BW_LABEL).pdf

../output/treatment_control_map_2015_$(MAP_BW_LABEL)_data.csv: ../../../event_study_treatment_maps/code/run_diagnostics.R ../../../_lib/border_pair_helpers.R ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet ../../../create_event_study_permit_data/output/permit_block_year_panel_2023.parquet ../../../merge_event_study_scores/output/block_treatment_panel.csv ../../../census_block_2020_cleaning/output/census_blocks_2020.csv ../../../ward_panel_create/output/ward_panel.gpkg ../../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../output
	$(MAKE) -C ../../../event_study_treatment_maps/code link-inputs
	cd ../../../event_study_treatment_maps/code && WRITE_MAP_AUDIT=1 MAP_AUDIT_OUTPUT_DIR=../../audits/event_study_treatment_maps_audit/output BANDWIDTH_LABEL=$(MAP_BW_LABEL) Rscript run_diagnostics.R $(MAP_BW_M)

../output/treatment_control_map_2023_$(MAP_BW_LABEL)_data.csv \
../output/treatment_control_map_2023_$(MAP_BW_LABEL).pdf \
../output/ward_pair_before_after_13-23_$(MAP_BW_LABEL).pdf: ../output/treatment_control_map_2015_$(MAP_BW_LABEL)_data.csv
	@test -s "$@"

link-inputs:

include ../../../shell_functions.make
include ../../../generic.make
