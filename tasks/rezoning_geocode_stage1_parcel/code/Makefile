SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
REZONING_DATE_TAG ?= 19990101_20260212
REZONING_INPUT_TASK ?= rezoning_far_pre_geocode
REZONING_INPUT_BASENAME ?= $(if $(filter rezoning_far_pre_geocode,$(REZONING_INPUT_TASK)),zoning_matters_far,zoning_matters)
FAR_GATE_REQUIRED := $(filter rezoning_far_pre_geocode,$(REZONING_INPUT_TASK))
PARCEL_UNIVERSE_DATASET_ID ?= pabr-t5kh
PARCEL_ADDRESSES_DATASET_ID ?= 3723-97qp
NEAREST_MAX_DIFF ?= 50

all: \
	../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv

diagnostics: ../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv
handoff: ../output/chicago_geocoder_upload_$(REZONING_DATE_TAG).xlsx ../output/chicago_geocoder_upload_mapping_$(REZONING_DATE_TAG).csv
census-template: ../output/census_geocoder_upload_template_$(REZONING_DATE_TAG).csv

link-inputs: ../input/$(REZONING_INPUT_BASENAME)_$(REZONING_DATE_TAG).csv ../input/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv ../input/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv $(if $(FAR_GATE_REQUIRED),../input/rezoning_far_pre_geocode_summary_$(REZONING_DATE_TAG).json,)

../input/$(REZONING_INPUT_BASENAME)_$(REZONING_DATE_TAG).csv: ../../$(REZONING_INPUT_TASK)/output/$(REZONING_INPUT_BASENAME)_$(REZONING_DATE_TAG).csv | ../input
	ln -sf "$<" "$@"

../input/rezoning_far_pre_geocode_summary_$(REZONING_DATE_TAG).json: ../../rezoning_far_pre_geocode/output/rezoning_far_pre_geocode_summary_$(REZONING_DATE_TAG).json | ../input
	ln -sf "$<" "$@"

../input/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv: ../../cook_parcel_addresses_download/output/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv | ../input
	ln -sf "$<" "$@"

../input/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv: ../../cook_parcel_addresses_download/output/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv | ../input
	ln -sf "$<" "$@"

../output/far_gate_ok_$(REZONING_DATE_TAG).json: check_far_gate.py ../input/rezoning_far_pre_geocode_summary_$(REZONING_DATE_TAG).json | ../output run.sbatch
	$(PYTHON) check_far_gate.py \
		--in-summary-json ../input/rezoning_far_pre_geocode_summary_$(REZONING_DATE_TAG).json \
		--out-stamp ../output/far_gate_ok_$(REZONING_DATE_TAG).json

ifeq ($(FAR_GATE_REQUIRED),)
../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv: stage1_parcel_geocode.py ../input/$(REZONING_INPUT_BASENAME)_$(REZONING_DATE_TAG).csv ../input/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv ../input/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv | ../output run.sbatch
else
../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv: ../output/far_gate_ok_$(REZONING_DATE_TAG).json stage1_parcel_geocode.py ../input/$(REZONING_INPUT_BASENAME)_$(REZONING_DATE_TAG).csv ../input/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv ../input/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv | ../output run.sbatch
endif
	$(PYTHON) stage1_parcel_geocode.py \
		--in-rezoning-csv ../input/$(REZONING_INPUT_BASENAME)_$(REZONING_DATE_TAG).csv \
		--in-parcel-addresses-csv ../input/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv \
		--in-parcel-universe-csv ../input/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv \
		--nearest-max-diff $(NEAREST_MAX_DIFF) \
		--out-stage1-csv ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv \
		--out-unmatched-csv ../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv

../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv: ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv

../output/chicago_geocoder_upload_$(REZONING_DATE_TAG).xlsx: build_chicago_upload_workbook.py ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv ../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv | ../output run.sbatch
	$(PYTHON) build_chicago_upload_workbook.py \
		--in-unmatched-csv ../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv \
		--in-stage1-csv ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv \
		--out-upload-xlsx ../output/chicago_geocoder_upload_$(REZONING_DATE_TAG).xlsx \
		--out-mapping-csv ../output/chicago_geocoder_upload_mapping_$(REZONING_DATE_TAG).csv

../output/chicago_geocoder_upload_mapping_$(REZONING_DATE_TAG).csv: ../output/chicago_geocoder_upload_$(REZONING_DATE_TAG).xlsx

../output/census_geocoder_upload_template_$(REZONING_DATE_TAG).csv: build_census_template.R ../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv | ../output run.sbatch
	$(R) build_census_template.R \
		--date-tag $(REZONING_DATE_TAG)

test: test_stage1_parcel_geocode.py test_build_chicago_upload_workbook.py test_check_far_gate.py stage1_parcel_geocode.py build_chicago_upload_workbook.py check_far_gate.py build_census_template.R
	$(PYTHON) -m unittest test_stage1_parcel_geocode.py test_build_chicago_upload_workbook.py test_check_far_gate.py
