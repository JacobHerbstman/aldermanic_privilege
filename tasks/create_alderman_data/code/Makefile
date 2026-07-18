ALDERMAN_PANEL_END_MONTH ?= 2026-04

all: ../output/chicago_alderman_panel.csv ../output/chicago_alderman_terms.csv

../output/chicago_alderman_panel.csv: create_alderman_panel.R | ../output
	Rscript $< $(ALDERMAN_PANEL_END_MONTH)

../output/chicago_alderman_terms.csv: ../output/chicago_alderman_panel.csv
	@test -s "$@"

include ../../generic.make
