SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
export PYTHONDONTWRITEBYTECODE=1
YEAR_START ?= 1977
YEAR_END ?= 2009
SLEEP_SECONDS ?= 2.5
TIMEOUT_SECONDS ?= 120
MAX_RETRIES ?= 1

YEAR_TAG := $(YEAR_START)_$(YEAR_END)

all: ../output/journal_manifest_$(YEAR_TAG).csv

link-inputs:

../temp/journal_manifest_scraped_$(YEAR_TAG).csv: scrape_journal_manifest.py journal_parsers.py | ../temp run.sbatch
	$(PYTHON) $< \
		--year-start $(YEAR_START) \
		--year-end $(YEAR_END) \
		--out-manifest-csv ../temp/journal_manifest_scraped_$(YEAR_TAG).csv

../output/journal_manifest_$(YEAR_TAG).csv: download_validate_profile.py journal_parsers.py ../temp/journal_manifest_scraped_$(YEAR_TAG).csv | ../output ../temp run.sbatch
	$(PYTHON) download_validate_profile.py \
		--in-manifest-csv ../temp/journal_manifest_scraped_$(YEAR_TAG).csv \
		--pdf-root-dir ../output/journals \
		--sleep-seconds $(SLEEP_SECONDS) \
		--timeout-seconds $(TIMEOUT_SECONDS) \
		--max-retries $(MAX_RETRIES) \
		--out-manifest-csv ../output/journal_manifest_$(YEAR_TAG).csv

test: test_journal_parsers.py journal_parsers.py
	$(PYTHON) -m unittest test_journal_parsers.py
