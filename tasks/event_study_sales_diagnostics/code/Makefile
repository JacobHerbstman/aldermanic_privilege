SHELL := bash
include ../../shell_functions.make

all: \
	../output/treatment_control_map_2015.pdf \
	../output/treatment_control_maps_combined.pdf \
	../output/ward_pair_before_after_13-23.pdf \
	../output/ward_pair_vertical_13-23.pdf

../output/treatment_control_map_2015.pdf ../output/treatment_control_maps_combined.pdf ../output/ward_pair_before_after_13-23.pdf ../output/ward_pair_vertical_13-23.pdf: ../output/.diagnostics.done

../output/.diagnostics.done: run_diagnostics.R ../input/sales_stacked_panel.csv ../input/sales_stacked_quarterly_panel.csv ../input/census_blocks_2010.csv ../input/ward_panel.gpkg ../input/block_treatment_panel.csv | ../output
	$(R) run_diagnostics.R
	find ../output -maxdepth 1 -type f \
		! -name treatment_control_map_2015.pdf \
		! -name treatment_control_maps_combined.pdf \
		! -name ward_pair_before_after_13-23.pdf \
		! -name ward_pair_vertical_13-23.pdf \
		! -name .diagnostics.done \
		-delete
	touch "$@"

../output/treatment_summary_stats.csv: treatment_summary_stats.R ../input/block_treatment_panel.csv | ../output
	$(R) treatment_summary_stats.R
	rm -f ../output/treatment_summary_presentation.txt

../input/sales_stacked_panel.csv: ../../create_event_study_sales_data/output/sales_stacked_panel.csv | ../input
	ln -sf "$<" "$@"

../input/sales_stacked_quarterly_panel.csv: ../../create_event_study_sales_data/output/sales_stacked_quarterly_panel.csv | ../input
	ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../create_event_study_sales_data/input/census_blocks_2010.csv | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/block_treatment_panel.csv: ../../create_block_treatment_panel/output/block_treatment_panel.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/sales_stacked_panel.csv ../input/sales_stacked_quarterly_panel.csv ../input/census_blocks_2010.csv ../input/ward_panel.gpkg ../input/block_treatment_panel.csv

include ../../generic.make
