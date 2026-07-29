DATE_TAG ?= 20101101_20201231

all: ../output/section_review_matters.csv ../output/section_review_summary.csv

../output/section_review_matters.csv: audit_section_review.py ../input/zoning_matters_far_$(DATE_TAG).csv ../input/zoning_far_lookup_clean.csv ../input/pdf_text_$(DATE_TAG).csv ../../../rezoning_far_pre_geocode/code/assign_far_pre_geocode.py | ../output
	$(PYTHON) $< $(DATE_TAG)

../output/section_review_summary.csv: ../output/section_review_matters.csv
	@test -s "$@"

../input/zoning_matters_far_$(DATE_TAG).csv: ../../../rezoning_far_pre_geocode/output/zoning_matters_far_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/zoning_far_lookup_clean.csv: ../../../zoning_data_cleaning/output/zoning_far_lookup_clean.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/pdf_text_$(DATE_TAG).csv: ../../../elms_pdf_processing/output/pdf_text_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/zoning_matters_far_$(DATE_TAG).csv ../input/zoning_far_lookup_clean.csv ../input/pdf_text_$(DATE_TAG).csv

include ../../../shell_functions.make
include ../../../generic.make
