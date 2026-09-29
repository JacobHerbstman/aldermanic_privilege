include ../../shared/code/shell_functions.make

JOURNAL_YEARS := $(shell seq 2000 2011)

all: ../output/journal_amendment_follow_up.csv

../output/journal_amendment_follow_up.csv: follow_journal_amendments.R ../input/journal_zoning_outcomes.csv \
	$(foreach year,$(JOURNAL_YEARS),../input/journal_introductions_$(year).csv) ../input/journal_amendment_places.csv \
	../input/zoning_map_amendments.csv ../input/zoning_amendment_wards.csv ../input/zoning_conversion_2004_crosswalk.csv \
	../adjudication/journal_refiling_reviews.csv | ../output ../report
	$(R) $<

../input/journal_zoning_outcomes.csv: ../../link_journal_zoning_outcomes/output/journal_zoning_outcomes.csv | ../input
	ln -sf $< $@
../input/journal_introductions_%.csv: ../../parse_journal_zoning_amendments/output/journal_introductions_%.csv \
	| ../input
	ln -sf $< $@
../input/journal_amendment_places.csv: \
	../../place_journal_zoning_amendments/output/journal_amendment_places.csv | ../input
	ln -sf $< $@
../input/zoning_map_amendments.csv: ../../clean_zoning_map_amendments/output/zoning_map_amendments.csv | ../input
	ln -sf $< $@
../input/zoning_amendment_wards.csv: ../../assign_zoning_amendment_wards/output/zoning_amendment_wards.csv | ../input
	ln -sf $< $@
../input/zoning_conversion_2004_crosswalk.csv: ../../../data_raw/zoning_conversion_2004_crosswalk.csv | ../input
	ln -sf $< $@

link-inputs: ../input/journal_zoning_outcomes.csv \
	$(foreach year,$(JOURNAL_YEARS),../input/journal_introductions_$(year).csv) ../input/journal_amendment_places.csv \
	../input/zoning_map_amendments.csv ../input/zoning_amendment_wards.csv ../input/zoning_conversion_2004_crosswalk.csv

include ../../shared/code/generic.make
