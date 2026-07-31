ALDERMAN_PANEL_END_MONTH ?= 2022-12

all: ../output/chicago_alderman_panel.csv ../output/chicago_alderman_terms.csv

../output/chicago_alderman_panel.csv: create_alderman_panel.R Makefile | ../output
	$(R) $< $(ALDERMAN_PANEL_END_MONTH)

../output/chicago_alderman_terms.csv: ../output/chicago_alderman_panel.csv
	@test -s "$@"

include ../../shell_functions.make
include ../../generic.make
