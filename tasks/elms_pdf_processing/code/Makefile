START_DATE ?= 2010-11-01
END_DATE ?= 2026-02-12
SLEEP_SECONDS ?= 0.2
MAX_RETRIES ?= 4
MAX_ATTACHMENTS_PER_MATTER ?= 1
REQUEST_TIMEOUT_SECONDS ?= 120
PARSE_TIMEOUT_SECONDS ?= 180
PROGRESS_EVERY ?= 25
DATE_TAG := $(subst -,,$(START_DATE))_$(subst -,,$(END_DATE))

all: ../output/pdf_text_$(DATE_TAG).csv ../output/pdf_zoning_fields_$(DATE_TAG).csv

../output/pdf_text_$(DATE_TAG).csv: process_pdfs.py pdf_parsers.py ../input/matter_attachments_$(DATE_TAG).csv ../input/candidate_final_ids_$(DATE_TAG).csv | ../output ../temp run.sbatch
	$(PYTHON) $< \
		--in-attachments-csv ../input/matter_attachments_$(DATE_TAG).csv \
		--in-candidates-csv ../input/candidate_final_ids_$(DATE_TAG).csv \
		--pdf-dir ../output/pdfs_$(DATE_TAG) \
		--sleep-seconds $(SLEEP_SECONDS) \
		--max-retries $(MAX_RETRIES) \
		--max-attachments-per-matter $(MAX_ATTACHMENTS_PER_MATTER) \
		--request-timeout-seconds $(REQUEST_TIMEOUT_SECONDS) \
		--parse-timeout-seconds $(PARSE_TIMEOUT_SECONDS) \
		--progress-every $(PROGRESS_EVERY) \
		--out-text-csv ../output/pdf_text_$(DATE_TAG).csv \
		--out-fields-csv ../temp/pdf_zoning_fields_download_parse_$(DATE_TAG).csv

../output/pdf_zoning_fields_$(DATE_TAG).csv: parse_pdf_text_fields.py pdf_parsers.py ../output/pdf_text_$(DATE_TAG).csv | ../output
	$(PYTHON) $< $(DATE_TAG) --parse-timeout-seconds $(PARSE_TIMEOUT_SECONDS)

../input/matter_attachments_$(DATE_TAG).csv: ../../elms_enrich_matters/output/matter_attachments_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/candidate_final_ids_$(DATE_TAG).csv: ../../elms_enrich_matters/output/candidate_final_ids_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/matter_attachments_$(DATE_TAG).csv ../input/candidate_final_ids_$(DATE_TAG).csv

include ../../shell_functions.make
include ../../generic.make
