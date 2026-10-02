include ../../shared/code/shell_functions.make

ALDERMAN_PANEL_END_MONTH ?= 2022-12

all: ../output/chicago_alderman_terms.csv \
	../output/chicago_alderman_panel.csv

../%/chicago_alderman_panel.csv \
	../%/chicago_alderman_terms.csv: create_alderman_panel.R ../input/alderman_terms.csv Makefile | ../output ../report
	$(R) $< $(ALDERMAN_PANEL_END_MONTH)

../input/alderman_terms.csv: ../adjudication/alderman_terms.csv | ../input
	ln -sf $< $@

link-inputs: ../input/alderman_terms.csv

include ../../shared/code/generic.make
