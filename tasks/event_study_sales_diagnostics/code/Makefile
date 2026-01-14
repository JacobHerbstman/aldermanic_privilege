SHELL := bash
include ../../shell_functions.make

all: ../output/diagnostics.done

../output/diagnostics.done: run_diagnostics.R ../input/sales_stacked_panel.csv ../input/sales_stacked_quarterly_panel.csv ../input/census_blocks_2010.csv ../input/ward_panel.gpkg ../input/block_treatment_panel.csv | ../output
	$(R) run_diagnostics.R
	touch $@

../input/sales_stacked_panel.csv: ../../create_event_study_sales_data/output/sales_stacked_panel.csv | ../input
	ln -sf $< $@

../input/sales_stacked_quarterly_panel.csv: ../../create_event_study_sales_data/output/sales_stacked_quarterly_panel.csv | ../input
	ln -sf $< $@

../input/census_blocks_2010.csv: ../../create_event_study_sales_data/input/census_blocks_2010.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/block_treatment_panel.csv: ../../create_block_treatment_panel/output/block_treatment_panel.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/sales_stacked_panel.csv ../input/sales_stacked_quarterly_panel.csv ../input/census_blocks_2010.csv ../input/ward_panel.gpkg ../input/block_treatment_panel.csv
