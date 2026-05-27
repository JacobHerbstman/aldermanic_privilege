SHELL := bash

MAP_BW_M ?= 304.8
MAP_BW_LABEL ?= 1000ft

all: \
	../output/treatment_control_map_2015_$(MAP_BW_LABEL).pdf \
	../output/ward_pair_vertical_13-23_$(MAP_BW_LABEL).pdf

../output/treatment_control_map_2015_$(MAP_BW_LABEL).pdf: plot_treatment_maps.R ../input/permit_block_year_panel_2015.parquet ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv ../input/ward_panel.gpkg | ../output
	Rscript $< $(MAP_BW_M) $(MAP_BW_LABEL)

../output/ward_pair_vertical_13-23_$(MAP_BW_LABEL).pdf: ../output/treatment_control_map_2015_$(MAP_BW_LABEL).pdf
	@test -s "$@"

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/block_treatment_panel.csv: ../../merge_event_study_scores/output/block_treatment_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/census_blocks_2010.csv ../input/ward_panel.gpkg ../input/block_treatment_panel.csv

include ../../generic.make
