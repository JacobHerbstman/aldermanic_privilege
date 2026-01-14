SHELL := bash
include ../../shell_functions.make

all: ../output/block_group_controls.csv

../output/block_group_controls.csv: create_block_group_controls.R | ../output
	$(R) create_block_group_controls.R

include ../../generic.make