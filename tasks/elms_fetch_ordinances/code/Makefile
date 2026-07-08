SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
START_DATE ?= 2010-12-01
END_DATE ?= 2026-02-12
TOP ?= 500
SLEEP_SECONDS ?= 0.2
DATE_TAG := $(subst -,,$(START_DATE))_$(subst -,,$(END_DATE))

all: ../output/matters_$(DATE_TAG).csv ../output/candidate_seed_ids_$(DATE_TAG).csv

link-inputs:

../output/matters_$(DATE_TAG).csv ../output/candidate_seed_ids_$(DATE_TAG).csv: fetch_matters.py | ../output run.sbatch
	$(PYTHON) $< \
		--start-date $(START_DATE) \
		--end-date $(END_DATE) \
		--top $(TOP) \
		--sleep-seconds $(SLEEP_SECONDS) \
		--out-matters-csv ../output/matters_$(DATE_TAG).csv \
		--out-seed-csv ../output/candidate_seed_ids_$(DATE_TAG).csv

test: test_fetch_matters.py fetch_matters.py
	$(PYTHON) -m unittest test_fetch_matters.py
