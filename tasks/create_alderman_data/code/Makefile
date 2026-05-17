SHELL := bash

ALDERMAN_PANEL_END_MONTH ?= 2026-04

all: ../output/chicago_alderman_panel.csv ../output/chicago_alderman_panel_coverage.csv

../output/chicago_alderman_panel.csv: create_alderman_panel.R | ../output
	ALDERMAN_PANEL_END_MONTH=$(ALDERMAN_PANEL_END_MONTH) Rscript $<

../output/chicago_alderman_panel_coverage.csv: ../output/chicago_alderman_panel.csv
	@test -s "$@"

include ../../shell_functions.make
include ../../generic.make
