SHELL := bash
include ../../shell_functions.make

all: ../output/ward_controls_2000_2023.csv ../output/block_group_controls_2000_2023.csv

../output/ward_controls_2000_2023.csv ../output/block_group_controls_2000_2023.csv: create_ward_controls.R ../input/ward_panel.gpkg | ../output
	$(R) $<

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/ward_panel.gpkg