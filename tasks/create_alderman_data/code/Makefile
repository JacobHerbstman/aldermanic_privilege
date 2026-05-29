ALDERMAN_PANEL_END_MONTH ?= 2026-04

all: ../output/chicago_alderman_panel.csv

../output/chicago_alderman_panel.csv: create_alderman_panel.R | ../output
	ALDERMAN_PANEL_END_MONTH=$(ALDERMAN_PANEL_END_MONTH) Rscript $<

include ../../generic.make
