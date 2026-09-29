include ../../shared/code/shell_functions.make

JOURNAL_YEARS := $(shell seq 2000 2011)

all: $(foreach year,$(JOURNAL_YEARS),../output/journal_zoning_ocr_$(year).parquet)

../output/journal_zoning_ocr_%.parquet: ocr_journal_zoning_pages.R ../input/journal_pages_%.parquet ../input/journals_% \
	| ../output ../report ../temp
	$(R) $< $*

../input/journal_pages_%.parquet: ../../extract_council_journal_text/output/journal_pages_%.parquet | ../input
	ln -sf $< $@
# The folder of Journals is complete once the download has written its file record.
../input/journals_%: ../../download_council_journals/output/council_journal_files_%.csv | ../input
	ln -sfn ../../download_council_journals/output/journals_$* $@

link-inputs: $(foreach year,$(JOURNAL_YEARS),../input/journal_pages_$(year).parquet ../input/journals_$(year))

include ../../shared/code/generic.make
