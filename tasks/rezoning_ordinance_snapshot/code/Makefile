SHELL := bash
include ../../shell_functions.make

all: ../output/chicago_council.db ../output/rezoning_ordinance_panel.csv ../output/rezoning_ordinance_documents.csv

../output/rezoning_ordinance_panel.csv: build_rezoning_ordinance_snapshot.R | ../output
	$(R) $<

../output/chicago_council.db ../output/rezoning_ordinance_documents.csv: ../output/rezoning_ordinance_panel.csv

include ../../generic.make

link-inputs:
