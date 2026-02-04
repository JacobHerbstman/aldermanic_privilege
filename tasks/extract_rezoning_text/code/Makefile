SHELL := bash
include ../../shell_functions.make

MAX_BILLS ?= 300

all: ../output/rezoning_text_extraction_sample.csv ../output/rezoning_address_sponsor_candidates.csv

../output/rezoning_text_extraction_sample.csv: extract_rezoning_text.R ../input/rezoning_ordinance_panel.csv ../input/rezoning_ordinance_documents.csv | ../output ../temp
	$(R) $< --max-bills $(MAX_BILLS)

../output/rezoning_address_sponsor_candidates.csv: ../output/rezoning_text_extraction_sample.csv

../input/rezoning_ordinance_panel.csv: ../../rezoning_ordinance_snapshot/output/rezoning_ordinance_panel.csv | ../input
	ln -sf $< $@

../input/rezoning_ordinance_documents.csv: ../../rezoning_ordinance_snapshot/output/rezoning_ordinance_documents.csv | ../input
	ln -sf $< $@

../../rezoning_ordinance_snapshot/output/rezoning_ordinance_panel.csv:
	$(MAKE) -C ../../rezoning_ordinance_snapshot/code ../output/rezoning_ordinance_panel.csv

../../rezoning_ordinance_snapshot/output/rezoning_ordinance_documents.csv: ../../rezoning_ordinance_snapshot/output/rezoning_ordinance_panel.csv

include ../../generic.make

link-inputs: ../input/rezoning_ordinance_panel.csv ../input/rezoning_ordinance_documents.csv
