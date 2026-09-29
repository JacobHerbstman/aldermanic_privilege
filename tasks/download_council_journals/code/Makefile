include ../../shared/code/shell_functions.make

JOURNAL_YEARS := $(shell seq 1981 2011)

all: $(foreach year,$(JOURNAL_YEARS),../output/council_journal_files_$(year).csv)

../output/council_journal_files_%.csv: download_council_journals.R | ../output/journals_% ../report ../temp
	$(R) $< $*

$(foreach year,$(JOURNAL_YEARS),../output/journals_$(year)): | ../output
	mkdir -p $@

include ../../shared/code/generic.make
