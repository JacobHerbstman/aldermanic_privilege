include ../../shared/code/shell_functions.make

JOURNAL_YEARS := $(shell seq 2000 2011)

all: $(foreach year,$(JOURNAL_YEARS),../output/journal_pages_$(year).parquet)

../output/journal_pages_%.parquet: extract_journal_text.R ../input/council_journal_files_%.csv ../input/journals_% \
	| ../output ../report
	$(R) $< $*

../input/council_journal_files_%.csv: ../../download_council_journals/output/council_journal_files_%.csv | ../input
	ln -sf $< $@
# The folder of Journals is complete once the download has written its file record.
../input/journals_%: ../../download_council_journals/output/council_journal_files_%.csv | ../input
	ln -sfn ../../download_council_journals/output/journals_$* $@

link-inputs: $(foreach year,$(JOURNAL_YEARS),../input/council_journal_files_$(year).csv ../input/journals_$(year))

include ../../shared/code/generic.make
