SHELL := bash

all: ../output/ward_controls_2000_2023.csv ../output/block_group_controls_2000_2023.csv ../output/ward_controls_census_metadata.csv

../output/ward_controls_2000_2023.csv: create_ward_controls.R ../input/ward_panel.gpkg | ../output
	$(R) $<
	test -f ../output/block_group_controls_2000_2023.csv
	test -f ../output/ward_controls_census_metadata.csv

../output/block_group_controls_2000_2023.csv: ../output/ward_controls_2000_2023.csv
	@test -s "$@"

../output/ward_controls_census_metadata.csv: ../output/ward_controls_2000_2023.csv
	@test -s "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"


link-inputs: ../input/ward_panel.gpkg

include ../../shell_functions.make
include ../../generic.make
