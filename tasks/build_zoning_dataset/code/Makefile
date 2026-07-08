SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
START_DATE ?= 2010-12-01
END_DATE ?= 2026-02-12
DATE_TAG := $(subst -,,$(START_DATE))_$(subst -,,$(END_DATE))

all: ../output/zoning_matters_$(DATE_TAG).csv

link-inputs: \
	../input/matters_$(DATE_TAG).csv \
	../input/matter_sponsors_$(DATE_TAG).csv \
	../input/matter_histories_$(DATE_TAG).csv \
	../input/matter_attachments_$(DATE_TAG).csv \
	../input/pdf_zoning_fields_$(DATE_TAG).csv \
	../input/pdf_text_$(DATE_TAG).csv \
	../input/candidate_final_ids_$(DATE_TAG).csv \
	../input/councilmatic_fallback_$(DATE_TAG).csv

../input/matters_$(DATE_TAG).csv: ../../elms_fetch_ordinances/output/matters_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/matter_sponsors_$(DATE_TAG).csv: ../../elms_enrich_matters/output/matter_sponsors_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/matter_histories_$(DATE_TAG).csv: ../../elms_enrich_matters/output/matter_histories_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/matter_attachments_$(DATE_TAG).csv: ../../elms_enrich_matters/output/matter_attachments_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/pdf_zoning_fields_$(DATE_TAG).csv: ../../elms_pdf_processing/output/pdf_zoning_fields_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/pdf_text_$(DATE_TAG).csv: ../../elms_pdf_processing/output/pdf_text_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/candidate_final_ids_$(DATE_TAG).csv: ../../elms_enrich_matters/output/candidate_final_ids_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/councilmatic_fallback_$(DATE_TAG).csv: ../../build_zoning_councilmatic_fallback/output/councilmatic_fallback_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../output/zoning_matters_$(DATE_TAG).csv: assemble_zoning_tables.py title_parsers.py \
	../input/matters_$(DATE_TAG).csv \
	../input/matter_sponsors_$(DATE_TAG).csv \
	../input/matter_histories_$(DATE_TAG).csv \
	../input/matter_attachments_$(DATE_TAG).csv \
	../input/pdf_zoning_fields_$(DATE_TAG).csv \
	../input/pdf_text_$(DATE_TAG).csv \
	../input/candidate_final_ids_$(DATE_TAG).csv \
	../input/councilmatic_fallback_$(DATE_TAG).csv \
	| ../output run.sbatch
	$(PYTHON) assemble_zoning_tables.py \
		--in-matters ../input/matters_$(DATE_TAG).csv \
		--in-sponsors ../input/matter_sponsors_$(DATE_TAG).csv \
		--in-histories ../input/matter_histories_$(DATE_TAG).csv \
		--in-attachments ../input/matter_attachments_$(DATE_TAG).csv \
		--in-pdf-fields ../input/pdf_zoning_fields_$(DATE_TAG).csv \
		--in-pdf-text ../input/pdf_text_$(DATE_TAG).csv \
		--in-candidates ../input/candidate_final_ids_$(DATE_TAG).csv \
		--in-councilmatic-fields ../input/councilmatic_fallback_$(DATE_TAG).csv \
		--out-csv ../output/zoning_matters_$(DATE_TAG).csv

test: test_title_parsers.py test_zoning_parsers.py test_assemble_zoning_tables.py title_parsers.py assemble_zoning_tables.py
	$(PYTHON) -m unittest test_title_parsers.py test_zoning_parsers.py test_assemble_zoning_tables.py
