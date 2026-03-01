SHELL := bash
include ../../shell_functions.make

all: ../output/sales_block_year_panel.csv ../output/sales_block_month_panel.csv

../output/sales_block_year_panel.csv ../output/sales_block_month_panel.csv: create_event_study_data.R ../input/sales_with_ward_distances.csv ../input/ward_panel.gpkg ../input/census_blocks_2010.csv ../input/block_treatment_panel.csv ../input/block_group_controls.csv | ../output
	Rscript create_event_study_data.R

../input/sales_with_ward_distances.csv: ../../calculate_sale_distances/output/sales_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf "$<" "$@"

../input/block_treatment_panel.csv: ../../create_block_treatment_panel/output/block_treatment_panel.csv | ../input
	ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/sales_with_ward_distances.csv ../input/ward_panel.gpkg ../input/census_blocks_2010.csv ../input/block_treatment_panel.csv ../input/block_group_controls.csv

include ../../generic.make
