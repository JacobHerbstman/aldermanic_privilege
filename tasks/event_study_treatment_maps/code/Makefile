SHELL := bash

MAP_BW_M ?= 304.8
MAP_BW_LABEL ?= 1000ft

all: \
	../output/treatment_control_map_2015_$(MAP_BW_LABEL).pdf \
	../output/treatment_control_map_2023_$(MAP_BW_LABEL).pdf \
	../output/treatment_control_maps_combined_$(MAP_BW_LABEL).pdf \
	../output/ward_pair_before_after_13-23_$(MAP_BW_LABEL).pdf \
	../output/ward_pair_vertical_13-23_$(MAP_BW_LABEL).pdf

../output/treatment_control_maps_combined_$(MAP_BW_LABEL).pdf: run_diagnostics.R ../input/permit_block_year_panel_2015.parquet ../input/permit_block_year_panel_2023.parquet ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/ward_panel.gpkg | ../output
	@$(FUNCTIONS); BANDWIDTH_LABEL=$(MAP_BW_LABEL) R_pc_and_slurm run_diagnostics.R $(MAP_BW_M)

../output/treatment_control_map_2015_$(MAP_BW_LABEL).pdf \
../output/treatment_control_map_2023_$(MAP_BW_LABEL).pdf \
../output/ward_pair_before_after_13-23_$(MAP_BW_LABEL).pdf \
../output/ward_pair_vertical_13-23_$(MAP_BW_LABEL).pdf: ../output/treatment_control_maps_combined_$(MAP_BW_LABEL).pdf
	@test -s "$@"

../output/treatment_summary_stats.csv: treatment_summary_stats.R ../input/block_treatment_panel.csv | ../output
	$(R) treatment_summary_stats.R
	rm -f ../output/treatment_summary_presentation.txt

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2023.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2023.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2020.csv: ../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_treatment_panel.csv: ../../merge_event_study_scores/output/block_treatment_panel.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/permit_block_year_panel_2023.parquet ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/ward_panel.gpkg ../input/block_treatment_panel.csv

include ../../shell_functions.make
include ../../generic.make
