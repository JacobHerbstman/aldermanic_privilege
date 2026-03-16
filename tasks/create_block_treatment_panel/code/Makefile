SHELL := bash
include ../../shell_functions.make

all: ../output/block_treatment_pre_scores.csv ../output/block_treatment_geometry_diagnostics.csv

../output/block_treatment_pre_scores.csv: create_treatment_panel.R ../../_lib/canonical_geometry_helpers.R ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv | ../output
	$(R) create_treatment_panel.R

../output/block_treatment_geometry_diagnostics.csv: ../output/block_treatment_pre_scores.csv
	test -f $@

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

../input/census_blocks_2020.csv: ../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv
