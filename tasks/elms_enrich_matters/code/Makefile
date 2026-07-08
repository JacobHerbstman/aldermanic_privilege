SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
START_DATE ?= 2010-12-01
END_DATE ?= 2026-02-12
SLEEP_SECONDS ?= 0.2
MAX_RETRIES ?= 4
TIMEOUT_SECONDS ?= 60
DATE_TAG := $(subst -,,$(START_DATE))_$(subst -,,$(END_DATE))

all: \
	../output/matter_sponsors_$(DATE_TAG).csv \
	../output/matter_histories_$(DATE_TAG).csv \
	../output/matter_attachments_$(DATE_TAG).csv \
	../output/candidate_final_ids_$(DATE_TAG).csv

link-inputs: ../input/matters_$(DATE_TAG).csv ../input/candidate_seed_ids_$(DATE_TAG).csv

../input/matters_$(DATE_TAG).csv: ../../elms_fetch_ordinances/output/matters_$(DATE_TAG).csv | ../input
	ln -sf "$<" "$@"

../input/candidate_seed_ids_$(DATE_TAG).csv: ../../elms_fetch_ordinances/output/candidate_seed_ids_$(DATE_TAG).csv | ../input
	ln -sf "$<" "$@"

../temp/matter_children_long_$(DATE_TAG).csv: fetch_matter_children_long.py ../input/matters_$(DATE_TAG).csv ../input/candidate_seed_ids_$(DATE_TAG).csv | ../temp ../output run.sbatch
	$(PYTHON) fetch_matter_children_long.py \
		--date-tag $(DATE_TAG) \
		--sleep-seconds $(SLEEP_SECONDS) \
		--max-retries $(MAX_RETRIES) \
		--timeout-seconds $(TIMEOUT_SECONDS)

../output/matter_sponsors_$(DATE_TAG).csv ../output/matter_histories_$(DATE_TAG).csv: fetch_matter_children_sponsors_histories.R ../temp/matter_children_long_$(DATE_TAG).csv | ../output run.sbatch
	$(R) fetch_matter_children_sponsors_histories.R \
		--date-tag $(DATE_TAG)

../output/matter_attachments_$(DATE_TAG).csv ../output/candidate_final_ids_$(DATE_TAG).csv: fetch_matter_children_attachments_candidates.R ../temp/matter_children_long_$(DATE_TAG).csv | ../output run.sbatch
	$(R) fetch_matter_children_attachments_candidates.R \
		--date-tag $(DATE_TAG)

test: test_fetch_matter_children.py fetch_matter_children_long.py fetch_matter_children_sponsors_histories.R fetch_matter_children_attachments_candidates.R
	$(PYTHON) -m unittest test_fetch_matter_children.py
