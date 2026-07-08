SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
REZONING_DATE_TAG ?= 19990101_20260212

all: ../output/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv

diagnostics: ../output/rezoning_geocode_remaining_unmatched_$(REZONING_DATE_TAG).csv

link-inputs: ../input/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv ../input/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv ../input/chicago_geocoder_upload_mapping_$(REZONING_DATE_TAG).csv ../input/chicago_geocoder_results_$(REZONING_DATE_TAG).csv ../input/census_geocoder_results_$(REZONING_DATE_TAG).csv

../input/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv: ../../rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv | ../input
	@target="$$(readlink "$@" 2>/dev/null || true)"; \
	if [ "$$target" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv: ../../rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv | ../input
	@target="$$(readlink "$@" 2>/dev/null || true)"; \
	if [ "$$target" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/chicago_geocoder_upload_mapping_$(REZONING_DATE_TAG).csv: ../../rezoning_geocode_stage1_parcel/output/chicago_geocoder_upload_mapping_$(REZONING_DATE_TAG).csv | ../input
	@target="$$(readlink "$@" 2>/dev/null || true)"; \
	if [ "$$target" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/chicago_geocoder_results_$(REZONING_DATE_TAG).csv: ../../rezoning_geocode_external_merge_ready/output/chicago_geocoder_results_$(REZONING_DATE_TAG).csv | ../input
	@target="$$(readlink "$@" 2>/dev/null || true)"; \
	if [ "$$target" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/census_geocoder_results_$(REZONING_DATE_TAG).csv: ../../rezoning_geocode_external_merge_ready/output/census_geocoder_results_$(REZONING_DATE_TAG).csv | ../input
	@target="$$(readlink "$@" 2>/dev/null || true)"; \
	if [ "$$target" != "$<" ]; then ln -sf "$<" "$@"; fi

../output/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv: merge_external_geocoders.R ../input/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv ../input/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv ../input/chicago_geocoder_upload_mapping_$(REZONING_DATE_TAG).csv ../input/chicago_geocoder_results_$(REZONING_DATE_TAG).csv ../input/census_geocoder_results_$(REZONING_DATE_TAG).csv | ../output run.sbatch
	$(R) $< \
		--date-tag $(REZONING_DATE_TAG)

../output/rezoning_geocode_remaining_unmatched_$(REZONING_DATE_TAG).csv: merge_external_geocoders.R ../input/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv ../input/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv ../input/chicago_geocoder_upload_mapping_$(REZONING_DATE_TAG).csv ../input/chicago_geocoder_results_$(REZONING_DATE_TAG).csv ../input/census_geocoder_results_$(REZONING_DATE_TAG).csv | ../output run.sbatch
	$(R) $< \
		--date-tag $(REZONING_DATE_TAG) \
		--write-remaining-unmatched

test: test_merge_external_geocoders.py merge_external_geocoders.py merge_external_geocoders.R convert_chicago_bulkgeo_xlsx.R
	$(PYTHON) -m unittest test_merge_external_geocoders.py
	$(R) -e "parse(file='merge_external_geocoders.R'); parse(file='convert_chicago_bulkgeo_xlsx.R'); cat('OK\n')"

import-chicago-xlsx: convert_chicago_bulkgeo_xlsx.R ../input/chicago_geocoder_results_$(REZONING_DATE_TAG).xlsx
	$(R) $< \
		--date-tag $(REZONING_DATE_TAG)
