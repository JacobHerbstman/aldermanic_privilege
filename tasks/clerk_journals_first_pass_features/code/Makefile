YEAR_START ?= 1999
YEAR_END ?= 2010
USE_ZONING_START_PAGE_WINDOW ?= 1
ZONING_START_PAGE_BUFFER ?= 3
MAX_PAGES_AFTER_ZONING_START ?= 120
FALLBACK_MAX_PAGES_NO_START ?= 120
MAX_PDFS ?= 0
YEAR_TAG := $(YEAR_START)_$(YEAR_END)

all: ../output/journal_rezonings_first_pass_$(YEAR_TAG).csv

../output/journal_rezonings_first_pass_$(YEAR_TAG).csv: parse_journal_first_pass_features.py rezoning_parsers.py ../input/journal_manifest_$(YEAR_TAG).csv ../input/journals_$(YEAR_TAG) ../input/zoning_far_lookup_clean.csv | ../output run.sbatch
	$(PYTHON) $< \
		--in-manifest-csv ../input/journal_manifest_$(YEAR_TAG).csv \
		--in-journals-dir ../input/journals_$(YEAR_TAG) \
		--in-zoning-lookup-csv ../input/zoning_far_lookup_clean.csv \
		--year-start $(YEAR_START) \
		--year-end $(YEAR_END) \
		--use-zoning-start-page-window $(USE_ZONING_START_PAGE_WINDOW) \
		--zoning-start-page-buffer $(ZONING_START_PAGE_BUFFER) \
		--max-pages-after-zoning-start $(MAX_PAGES_AFTER_ZONING_START) \
		--fallback-max-pages-no-start $(FALLBACK_MAX_PAGES_NO_START) \
		--max-pdfs $(MAX_PDFS) \
		--out-features-csv ../output/journal_rezonings_first_pass_$(YEAR_TAG).csv

../input/journal_manifest_$(YEAR_TAG).csv: ../../clerk_journals_download/output/journal_manifest_$(YEAR_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/journals_$(YEAR_TAG): ../../clerk_journals_download/output/journals | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -snf "$<" "$@"

../input/zoning_far_lookup_clean.csv: ../../zoning_data_cleaning/output/zoning_far_lookup_clean.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/journal_manifest_$(YEAR_TAG).csv ../input/journals_$(YEAR_TAG) ../input/zoning_far_lookup_clean.csv

include ../../shell_functions.make
include ../../generic.make
