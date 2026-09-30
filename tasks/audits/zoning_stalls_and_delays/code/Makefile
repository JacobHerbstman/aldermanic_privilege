include ../../../shared/code/shell_functions.make

JOURNAL_YEARS := $(shell seq 2000 2011)

all: ../output/stalls_and_delays_by_year.png ../output/stalls_and_delays_by_alderman.csv \
	../output/large_project_matches.csv ../output/substitute_changes_by_alderman.csv ../output/substitute_change_tests.csv

../output/stalls_and_delays_by_year.png: plot_by_year.R ../output/stalls_and_delays_by_year.csv | ../output
	$(R) $<

../output/stalls_and_delays_by_year.csv: summarize_by_year.R ../output/zoning_amendments.csv | ../output ../report
	$(R) $<

../output/stalls_and_delays_by_alderman.csv: summarize_by_alderman.R ../output/zoning_amendments.csv \
	| ../output ../report
	$(R) $<

../output/large_project_matches.csv: match_large_projects.R ../output/zoning_amendments.csv \
	../input/zoning_map_amendments.csv ../input/zoning_amendment_wards.csv ../input/application_form_fields.csv \
	| ../output ../report
	$(R) $<

../output/substitute_changes_by_alderman.csv: summarize_substitute_changes.R ../output/zoning_amendments.csv \
	../input/substitute_changes.csv | ../output ../report
	$(R) $<

../output/substitute_change_tests.csv: test_substitute_changes.R ../output/zoning_amendments.csv \
	../input/substitute_changes.csv | ../output ../report
	$(R) $<

../output/zoning_amendments.csv: combine_amendments.R ../input/journal_amendment_follow_up.csv \
	../input/journal_amendment_places.csv $(foreach year,$(JOURNAL_YEARS),../input/journal_introductions_$(year).csv) \
	../input/zoning-code-summary-district-types.csv ../input/zoning_conversion_2004_crosswalk.csv \
	../input/zoning_map_amendments.csv ../input/zoning_amendment_wards.csv ../input/zoning_refilings.csv \
	../input/alderman_terms.csv | ../output ../report
	$(R) $<

../input/journal_amendment_follow_up.csv: \
	../../../follow_journal_zoning_amendments/output/journal_amendment_follow_up.csv | ../input
	ln -sf $< $@
../input/journal_amendment_places.csv: \
	../../../place_journal_zoning_amendments/output/journal_amendment_places.csv | ../input
	ln -sf $< $@
../input/journal_introductions_%.csv: ../../../parse_journal_zoning_amendments/output/journal_introductions_%.csv \
	| ../input
	ln -sf $< $@
../input/zoning-code-summary-district-types.csv: \
	../../../download_second_city_zoning_districts/output/zoning-code-summary-district-types.csv | ../input
	ln -sf $< $@
../input/zoning_conversion_2004_crosswalk.csv: ../../../../data_raw/zoning_conversion_2004_crosswalk.csv | ../input
	ln -sf $< $@
../input/zoning_map_amendments.csv: ../../../clean_zoning_map_amendments/output/zoning_map_amendments.csv | ../input
	ln -sf $< $@
../input/zoning_amendment_wards.csv: ../../../assign_zoning_amendment_wards/output/zoning_amendment_wards.csv | ../input
	ln -sf $< $@
../input/zoning_refilings.csv: ../../../link_zoning_refilings/output/zoning_refilings.csv | ../input
	ln -sf $< $@
../input/application_form_fields.csv: \
	../../../extract_zoning_application_forms/output/application_form_fields.csv | ../input
	ln -sf $< $@
../input/alderman_terms.csv: ../../../create_alderman_data/adjudication/alderman_terms.csv | ../input
	ln -sf $< $@
../input/substitute_changes.csv: ../../../compare_substitute_ordinances/output/substitute_changes.csv | ../input
	ln -sf $< $@

link-inputs: ../input/journal_amendment_follow_up.csv ../input/journal_amendment_places.csv \
	$(foreach year,$(JOURNAL_YEARS),../input/journal_introductions_$(year).csv) \
	../input/zoning-code-summary-district-types.csv ../input/zoning_conversion_2004_crosswalk.csv \
	../input/zoning_map_amendments.csv ../input/zoning_amendment_wards.csv ../input/zoning_refilings.csv \
	../input/alderman_terms.csv ../input/application_form_fields.csv ../input/substitute_changes.csv

include ../../../shared/code/generic.make
