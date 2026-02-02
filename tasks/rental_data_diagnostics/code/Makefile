SHELL := bash
include ../../shell_functions.make

INPUT_PANEL := ../input/chicago_rent_panel.parquet

all: ../output/diagnostics.done

../output/diagnostics.done: run_rental_diagnostics.R $(INPUT_PANEL) | ../output
	$(R) run_rental_diagnostics.R
	touch $@

$(INPUT_PANEL): ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	ln -sf $< $@

link-inputs: $(INPUT_PANEL)

include ../../generic.make
