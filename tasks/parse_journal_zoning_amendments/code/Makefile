include ../../shared/code/shell_functions.make

JOURNAL_YEARS := $(shell seq 2000 2011)

all: $(foreach year,$(JOURNAL_YEARS),../output/journal_introductions_$(year).csv ../output/journal_ordinances_$(year).csv \
	../output/journal_report_notes_$(year).csv)

../output/journal_introductions_%.csv ../output/journal_ordinances_%.csv ../output/journal_report_notes_%.csv: \
	parse_journal_zoning.R \
	../input/journal_pages_%.parquet ../input/journal_zoning_ocr_%.parquet \
	../input/zoning-code-summary-district-types.csv ../input/zoning_conversion_2004_crosswalk.csv \
	../adjudication/journal_parse_checks.csv | ../output ../report
	$(R) $< $*

../input/journal_pages_%.parquet: ../../extract_council_journal_text/output/journal_pages_%.parquet | ../input
	ln -sf $< $@
../input/journal_zoning_ocr_%.parquet: ../../ocr_council_journal_zoning_pages/output/journal_zoning_ocr_%.parquet | ../input
	ln -sf $< $@
../input/zoning-code-summary-district-types.csv: ../../download_second_city_zoning_districts/output/zoning-code-summary-district-types.csv | ../input
	ln -sf $< $@
../input/zoning_conversion_2004_crosswalk.csv: ../../../data_raw/zoning_conversion_2004_crosswalk.csv | ../input
	ln -sf $< $@

link-inputs: $(foreach year,$(JOURNAL_YEARS),../input/journal_pages_$(year).parquet \
	../input/journal_zoning_ocr_$(year).parquet) ../input/zoning-code-summary-district-types.csv \
	../input/zoning_conversion_2004_crosswalk.csv

include ../../shared/code/generic.make
