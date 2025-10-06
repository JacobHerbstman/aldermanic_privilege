SHELL := bash
include ../../shell_functions.make

# Default: build the monthly alderman panel
all: output/chicago_alderman_panel.csv

include ../../generic.make 

# File target: runs the R script from ./code so relative paths in the script work
output/chicago_alderman_panel.csv: create_alderman_panel.R | ../output
	$(R) $<

