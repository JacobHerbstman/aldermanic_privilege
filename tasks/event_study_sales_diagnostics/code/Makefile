SHELL := bash
include ../../shell_functions.make

SCRIPT := run_diagnostics.R
SUMMARY_SCRIPT := treatment_summary_stats.R

DIAG_INPUTS := ../input/sales_stacked_panel.csv ../input/sales_stacked_quarterly_panel.csv ../input/census_blocks_2010.csv ../input/ward_panel.gpkg ../input/block_treatment_panel.csv

MAP_2015 := ../output/treatment_control_map_2015.pdf
MAP_2023 := ../output/treatment_control_map_2023.pdf
PAIR_13_23 := ../output/ward_pair_before_after_13-23.pdf
SUMMARY_CSV := ../output/treatment_summary_stats.csv
STAMP := ../output/.diagnostics.done

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(MAP_2015) $(MAP_2023) $(PAIR_13_23) $(SUMMARY_CSV)

$(MAP_2015) $(MAP_2023) $(PAIR_13_23): $(STAMP)

$(STAMP): $(SCRIPT) $(DIAG_INPUTS) | ../output
	$(R) $(SCRIPT)
	find ../output -maxdepth 1 -type f \
		! -name "$(notdir $(MAP_2015))" \
		! -name "$(notdir $(MAP_2023))" \
		! -name "$(notdir $(PAIR_13_23))" \
		! -name "$(notdir $(STAMP))" \
		-delete
	touch "$@"

$(SUMMARY_CSV): $(SUMMARY_SCRIPT) ../input/block_treatment_panel.csv | ../output
	$(R) $(SUMMARY_SCRIPT)
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

link-inputs: $(DIAG_INPUTS)

include ../../generic.make
