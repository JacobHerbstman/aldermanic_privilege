SHELL := bash
include ../../shell_functions.make

all: ../output/chicago_alderman_panel.csv

include ../../generic.make

../output/chicago_alderman_panel.csv: create_alderman_panel.R | ../output
	$(R) $<
