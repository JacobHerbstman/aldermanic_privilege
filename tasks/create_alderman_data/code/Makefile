SHELL := bash

all: ../output/chicago_alderman_panel.csv


../output/chicago_alderman_panel.csv: create_alderman_panel.R | ../output
	$(R) $<

include ../../shell_functions.make
include ../../generic.make
