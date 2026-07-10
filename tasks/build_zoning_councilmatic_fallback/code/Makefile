START_DATE ?= 2010-11-01
END_DATE ?= 2026-02-12
COUNCILMATIC_SLEEP_SECONDS ?= 0
COUNCILMATIC_TIMEOUT_SECONDS ?= 30
COUNCILMATIC_PROGRESS_EVERY ?= 25
DATE_TAG := $(subst -,,$(START_DATE))_$(subst -,,$(END_DATE))

all: ../output/councilmatic_fallback_$(DATE_TAG).csv

../output/councilmatic_fallback_$(DATE_TAG).csv: build_councilmatic_fallback.py ../../build_zoning_dataset/code/assemble_zoning_tables.py ../../build_zoning_dataset/code/title_parsers.py ../input/matters_$(DATE_TAG).csv ../input/pdf_zoning_fields_$(DATE_TAG).csv ../input/pdf_text_$(DATE_TAG).csv ../input/candidate_final_ids_$(DATE_TAG).csv | ../output run.sbatch
	$(PYTHON) $< $(DATE_TAG) \
		--sleep-seconds $(COUNCILMATIC_SLEEP_SECONDS) \
		--timeout-seconds $(COUNCILMATIC_TIMEOUT_SECONDS) \
		--progress-every $(COUNCILMATIC_PROGRESS_EVERY)

../input/matters_$(DATE_TAG).csv: ../../elms_fetch_ordinances/output/matters_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/pdf_zoning_fields_$(DATE_TAG).csv: ../../elms_pdf_processing/output/pdf_zoning_fields_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/pdf_text_$(DATE_TAG).csv: ../../elms_pdf_processing/output/pdf_text_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/candidate_final_ids_$(DATE_TAG).csv: ../../elms_enrich_matters/output/candidate_final_ids_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/matters_$(DATE_TAG).csv ../input/pdf_zoning_fields_$(DATE_TAG).csv ../input/pdf_text_$(DATE_TAG).csv ../input/candidate_final_ids_$(DATE_TAG).csv

include ../../shell_functions.make
include ../../generic.make
