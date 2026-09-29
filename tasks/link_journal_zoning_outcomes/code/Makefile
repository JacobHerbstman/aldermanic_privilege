include ../../shared/code/shell_functions.make

JOURNAL_YEARS := $(shell seq 2000 2011)

all: ../output/journal_ordinance_links.csv ../output/journal_zoning_outcomes.csv

../%/journal_ordinance_links.csv ../%/journal_zoning_outcomes.csv: link_journal_outcomes.R \
	$(foreach year,$(JOURNAL_YEARS),../input/journal_introductions_$(year).csv ../input/journal_ordinances_$(year).csv \
	../input/journal_report_notes_$(year).csv) ../input/zoning_conversion_2004_crosswalk.csv | ../output ../report
	$(R) $<

../input/journal_%.csv: ../../parse_journal_zoning_amendments/output/journal_%.csv | ../input
	ln -sf $< $@
../input/zoning_conversion_2004_crosswalk.csv: ../../../data_raw/zoning_conversion_2004_crosswalk.csv | ../input
	ln -sf $< $@

link-inputs: $(foreach year,$(JOURNAL_YEARS),../input/journal_introductions_$(year).csv \
	../input/journal_ordinances_$(year).csv ../input/journal_report_notes_$(year).csv) \
	../input/zoning_conversion_2004_crosswalk.csv

include ../../shared/code/generic.make
