SHELL=bash

.DEFAULT_GOAL := all

PYTHON ?= python3
START_DATE ?= 2010-12-01
END_DATE ?= 2026-02-12
DATE_TAG := $(subst -,,$(START_DATE))_$(subst -,,$(END_DATE))
COUNCILMATIC_SLEEP_SECONDS ?= 0
COUNCILMATIC_TIMEOUT_SECONDS ?= 30
COUNCILMATIC_PROGRESS_EVERY ?= 25

all: ../output/councilmatic_fallback_$(DATE_TAG).csv

../output/councilmatic_fallback_$(DATE_TAG).csv: build_councilmatic_fallback.py \
	../../build_zoning_dataset/code/assemble_zoning_tables.py \
	../../build_zoning_dataset/code/title_parsers.py \
	../input/matters_$(DATE_TAG).csv \
	../input/pdf_zoning_fields_$(DATE_TAG).csv \
	../input/pdf_text_$(DATE_TAG).csv \
	../input/candidate_final_ids_$(DATE_TAG).csv \
	| ../output run.sbatch
	$(PYTHON) build_councilmatic_fallback.py $(DATE_TAG) \
		--sleep-seconds $(COUNCILMATIC_SLEEP_SECONDS) \
		--timeout-seconds $(COUNCILMATIC_TIMEOUT_SECONDS) \
		--progress-every $(COUNCILMATIC_PROGRESS_EVERY)

../input/matters_$(DATE_TAG).csv: ../../elms_fetch_ordinances/output/matters_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/pdf_zoning_fields_$(DATE_TAG).csv: ../../elms_pdf_processing/output/pdf_zoning_fields_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/pdf_text_$(DATE_TAG).csv: ../../elms_pdf_processing/output/pdf_text_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/candidate_final_ids_$(DATE_TAG).csv: ../../elms_enrich_matters/output/candidate_final_ids_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

link-inputs: \
	../input/matters_$(DATE_TAG).csv \
	../input/pdf_zoning_fields_$(DATE_TAG).csv \
	../input/pdf_text_$(DATE_TAG).csv \
	../input/candidate_final_ids_$(DATE_TAG).csv

include ../../shell_functions.make
include ../../generic.make
