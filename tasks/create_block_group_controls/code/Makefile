SHELL := bash
include ../../shell_functions.make

all: ../output/block_group_controls.csv ../output/block_group_geometry_2019.gpkg

../output/block_group_controls.csv: create_block_group_controls.R | ../output
	$(R) create_block_group_controls.R

../output/block_group_geometry_2019.gpkg: ../output/block_group_controls.csv
	test -f ../output/block_group_geometry_2019.gpkg

link-inputs:

include ../../generic.make
