include ../../../shared/code/shell_functions.make

SAMPLE_YEARS := $(shell seq 2000 2009)

all: ../output/journal_sample_pages.csv ../output/form_holdout_pages.csv ../output/journal_sample_scores.csv \
	../output/form_holdout_scores.csv ../output/passages_vs_elms.csv ../output/withdrawals_vs_elms.csv \
	../output/introductions_vs_elms.csv ../output/elms_coverage_by_month.csv ../output/journal_link_checks.csv \
	../output/outcomes_vs_elms.csv ../output/journal_places_vs_zoning_map.csv

../output/journal_sample_pages.csv: draw_journal_sample.R \
	$(foreach year,$(SAMPLE_YEARS),../input/journal_zoning_ocr_$(year).parquet) | ../output ../report
	$(R) $<

../output/form_holdout_pages.csv: draw_form_holdout.R ../input/zoning_map_amendments.csv \
	../input/application_form_reads.csv ../input/elms_zoning_legislation_files.csv \
	../input/application_form_pages.parquet | ../output ../report
	$(R) $<

../output/journal_sample_scores.csv: score_journal_sample.R ../output/journal_sample_pages.csv \
	../adjudication/journal_sample_introduction_reads.csv ../adjudication/journal_sample_passage_reads.csv \
	$(foreach year,$(SAMPLE_YEARS),../input/journal_introductions_$(year).csv ../input/journal_ordinances_$(year).csv) \
	../input/zoning-code-summary-district-types.csv ../input/zoning_conversion_2004_crosswalk.csv | ../output ../report
	$(R) $<

../output/form_holdout_scores.csv: score_form_holdout.R ../adjudication/form_holdout_reads.csv \
	../input/application_form_fields.csv | ../output ../report
	$(R) $<

../output/passages_vs_elms.csv: check_passages_against_elms.R ../input/journal_ordinances_2010.csv \
	../input/journal_ordinances_2011.csv ../input/zoning_map_amendments.csv | ../output ../report
	$(R) $<

../output/withdrawals_vs_elms.csv: check_withdrawals_against_elms.R ../input/journal_ordinances_2010.csv \
	../input/journal_ordinances_2011.csv ../input/journal_report_notes_2010.csv ../input/journal_report_notes_2011.csv \
	../input/zoning_map_amendments.csv | ../output ../report
	$(R) $<

../output/introductions_vs_elms.csv: check_introductions_against_elms.R ../input/journal_introductions_2010.csv \
	../input/journal_introductions_2011.csv ../input/zoning_map_amendments.csv | ../output ../report
	$(R) $<

../output/elms_coverage_by_month.csv: check_elms_coverage.R ../input/elms_matter_index.jsonl \
	../input/journal_introductions_2010.csv ../input/journal_introductions_2011.csv | ../output ../report
	$(R) $<

../output/journal_link_checks.csv: check_journal_links.R ../input/journal_ordinance_links.csv \
	../input/journal_zoning_outcomes.csv | ../output ../report
	$(R) $<

../output/outcomes_vs_elms.csv: check_outcomes_against_elms.R ../input/journal_zoning_outcomes.csv \
	../input/zoning_map_amendments.csv | ../output ../report
	$(R) $<

../output/journal_places_vs_zoning_map.csv: check_journal_places.R ../input/zoning_districts_current.geojson \
	../input/zoning_districts_2012.zip ../input/journal_ordinance_links.csv ../input/journal_amendment_places.csv \
	../input/Chicago_Wards_1998.geojson ../input/Wards_2014.geojson | ../output ../report
	$(R) $<

../input/journal_zoning_ocr_%.parquet: ../../../ocr_council_journal_zoning_pages/output/journal_zoning_ocr_%.parquet \
	| ../input
	ln -sf $< $@
../input/journal_introductions_%.csv: ../../../parse_journal_zoning_amendments/output/journal_introductions_%.csv \
	| ../input
	ln -sf $< $@
../input/journal_report_notes_%.csv: ../../../parse_journal_zoning_amendments/output/journal_report_notes_%.csv \
	| ../input
	ln -sf $< $@
../input/journal_ordinances_%.csv: ../../../parse_journal_zoning_amendments/output/journal_ordinances_%.csv | ../input
	ln -sf $< $@
../input/journal_ordinance_links.csv: ../../../link_journal_zoning_outcomes/output/journal_ordinance_links.csv \
	| ../input
	ln -sf $< $@
../input/journal_zoning_outcomes.csv: ../../../link_journal_zoning_outcomes/output/journal_zoning_outcomes.csv \
	| ../input
	ln -sf $< $@
../input/zoning_districts_current.geojson: \
	../../../download_chicago_gis_layers/output/zoning_districts_current.geojson | ../input
	ln -sf $< $@
../input/zoning_districts_2012.zip: ../../../download_chicago_gis_layers/output/zoning_districts_2012.zip | ../input
	ln -sf $< $@
../input/journal_amendment_places.csv: \
	../../../place_journal_zoning_amendments/output/journal_amendment_places.csv | ../input
	ln -sf $< $@
../input/Chicago_Wards_1998.geojson: ../../../../data_raw/Chicago_Wards_1998.geojson | ../input
	ln -sf $< $@
../input/Wards_2014.geojson: ../../../../data_raw/Wards_2014.geojson | ../input
	ln -sf $< $@
../input/zoning_conversion_2004_crosswalk.csv: ../../../../data_raw/zoning_conversion_2004_crosswalk.csv | ../input
	ln -sf $< $@
../input/zoning-code-summary-district-types.csv: \
	../../../download_second_city_zoning_districts/output/zoning-code-summary-district-types.csv | ../input
	ln -sf $< $@
../input/application_form_fields.csv: ../../../extract_zoning_application_forms/output/application_form_fields.csv \
	| ../input
	ln -sf $< $@
../input/zoning_map_amendments.csv: ../../../clean_zoning_map_amendments/output/zoning_map_amendments.csv | ../input
	ln -sf $< $@
../input/application_form_reads.csv: ../../../extract_zoning_application_forms/adjudication/application_form_reads.csv \
	| ../input
	ln -sf $< $@
../input/elms_matter_index.jsonl: ../../../download_elms_matters/output/elms_matter_index.jsonl | ../input
	ln -sf $< $@
../input/elms_zoning_legislation_files.csv: ../../../download_elms_matters/output/elms_zoning_legislation_files.csv \
	| ../input
	ln -sf $< $@
../input/application_form_pages.parquet: \
	../../../extract_zoning_application_forms/output/application_form_pages.parquet | ../input
	ln -sf $< $@

link-inputs: $(foreach year,$(SAMPLE_YEARS),../input/journal_zoning_ocr_$(year).parquet \
	../input/journal_introductions_$(year).csv ../input/journal_ordinances_$(year).csv) \
	../input/journal_introductions_2010.csv ../input/journal_introductions_2011.csv ../input/journal_ordinances_2010.csv \
	../input/journal_ordinances_2011.csv ../input/journal_report_notes_2010.csv ../input/journal_report_notes_2011.csv \
	../input/zoning-code-summary-district-types.csv ../input/zoning_conversion_2004_crosswalk.csv \
	../input/elms_matter_index.jsonl \
	../input/application_form_fields.csv ../input/zoning_map_amendments.csv ../input/application_form_reads.csv \
	../input/elms_zoning_legislation_files.csv ../input/application_form_pages.parquet \
	../input/journal_ordinance_links.csv ../input/journal_zoning_outcomes.csv ../input/zoning_districts_current.geojson \
	../input/zoning_districts_2012.zip ../input/journal_amendment_places.csv ../input/Chicago_Wards_1998.geojson \
	../input/Wards_2014.geojson

include ../../../shared/code/generic.make
