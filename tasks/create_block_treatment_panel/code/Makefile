SHELL := bash
include ../../shell_functions.make

all: ../output/block_treatment_panel.csv

../output/block_treatment_panel.csv: create_treatment_panel.R ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/alderman_restrictiveness_scores_month_FEs.csv | ../output
	$(R) create_treatment_panel.R

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

../input/census_blocks_2020.csv: ../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/alderman_restrictiveness_scores_month_FEs.csv: ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/alderman_restrictiveness_scores_month_FEs.csv
