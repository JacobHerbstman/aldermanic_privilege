SOURCE_START_DATE ?= 2010-11-01
SOURCE_END_DATE ?= 2026-02-12
SAMPLE_START_DATE ?= 2010-11-01
SAMPLE_END_DATE ?= 2020-12-31
SOURCE_TAG := $(subst -,,$(SOURCE_START_DATE))_$(subst -,,$(SOURCE_END_DATE))
SAMPLE_TAG := $(subst -,,$(SAMPLE_START_DATE))_$(subst -,,$(SAMPLE_END_DATE))

all: ../output/zoning_matters_$(SAMPLE_TAG).csv

../output/zoning_matters_$(SAMPLE_TAG).csv: assemble_zoning_tables.py title_parsers.py ../input/matters_$(SOURCE_TAG).csv ../input/matter_sponsors_$(SOURCE_TAG).csv ../input/matter_histories_$(SOURCE_TAG).csv ../input/matter_attachments_$(SOURCE_TAG).csv ../input/pdf_zoning_fields_$(SOURCE_TAG).csv ../input/pdf_text_$(SOURCE_TAG).csv ../input/candidate_final_ids_$(SOURCE_TAG).csv ../input/councilmatic_fallback_$(SOURCE_TAG).csv | ../output run.sbatch
	$(PYTHON) $< \
		--in-matters ../input/matters_$(SOURCE_TAG).csv \
		--in-sponsors ../input/matter_sponsors_$(SOURCE_TAG).csv \
		--in-histories ../input/matter_histories_$(SOURCE_TAG).csv \
		--in-attachments ../input/matter_attachments_$(SOURCE_TAG).csv \
		--in-pdf-fields ../input/pdf_zoning_fields_$(SOURCE_TAG).csv \
		--in-pdf-text ../input/pdf_text_$(SOURCE_TAG).csv \
		--in-candidates ../input/candidate_final_ids_$(SOURCE_TAG).csv \
		--in-councilmatic-fields ../input/councilmatic_fallback_$(SOURCE_TAG).csv \
		--sample-start-date $(SAMPLE_START_DATE) \
		--sample-end-date $(SAMPLE_END_DATE) \
		--out-csv ../output/zoning_matters_$(SAMPLE_TAG).csv

../input/matters_$(SOURCE_TAG).csv: ../../elms_fetch_ordinances/output/matters_$(SOURCE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/matter_sponsors_$(SOURCE_TAG).csv: ../../elms_enrich_matters/output/matter_sponsors_$(SOURCE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/matter_histories_$(SOURCE_TAG).csv: ../../elms_enrich_matters/output/matter_histories_$(SOURCE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/matter_attachments_$(SOURCE_TAG).csv: ../../elms_enrich_matters/output/matter_attachments_$(SOURCE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/pdf_zoning_fields_$(SOURCE_TAG).csv: ../../elms_pdf_processing/output/pdf_zoning_fields_$(SOURCE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/pdf_text_$(SOURCE_TAG).csv: ../../elms_pdf_processing/output/pdf_text_$(SOURCE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/candidate_final_ids_$(SOURCE_TAG).csv: ../../elms_enrich_matters/output/candidate_final_ids_$(SOURCE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/councilmatic_fallback_$(SOURCE_TAG).csv: ../../build_zoning_councilmatic_fallback/output/councilmatic_fallback_$(SOURCE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/matters_$(SOURCE_TAG).csv ../input/matter_sponsors_$(SOURCE_TAG).csv ../input/matter_histories_$(SOURCE_TAG).csv ../input/matter_attachments_$(SOURCE_TAG).csv ../input/pdf_zoning_fields_$(SOURCE_TAG).csv ../input/pdf_text_$(SOURCE_TAG).csv ../input/candidate_final_ids_$(SOURCE_TAG).csv ../input/councilmatic_fallback_$(SOURCE_TAG).csv

include ../../shell_functions.make
include ../../generic.make
