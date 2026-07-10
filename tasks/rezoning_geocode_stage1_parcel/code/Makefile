REZONING_DATE_TAG ?= 20101101_20201231
PARCEL_UNIVERSE_DATASET_ID ?= pabr-t5kh
PARCEL_ADDRESSES_DATASET_ID ?= 3723-97qp
NEAREST_MAX_DIFF ?= 50

all: ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv

../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv: stage1_parcel_geocode.py ../input/zoning_matters_far_$(REZONING_DATE_TAG).csv ../input/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv ../input/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv | ../output run.sbatch
	$(PYTHON) $< \
		--in-rezoning-csv ../input/zoning_matters_far_$(REZONING_DATE_TAG).csv \
		--in-parcel-addresses-csv ../input/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv \
		--in-parcel-universe-csv ../input/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv \
		--nearest-max-diff $(NEAREST_MAX_DIFF) \
		--out-stage1-csv ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv \
		--out-unmatched-csv ../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv

../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv: ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv

../output/chicago_geocoder_upload_$(REZONING_DATE_TAG).xlsx: build_chicago_upload_workbook.py ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv ../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv | ../output run.sbatch
	$(PYTHON) $< \
		--in-unmatched-csv ../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv \
		--in-stage1-csv ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv \
		--out-upload-xlsx ../output/chicago_geocoder_upload_$(REZONING_DATE_TAG).xlsx \
		--out-mapping-csv ../output/chicago_geocoder_upload_mapping_$(REZONING_DATE_TAG).csv

../output/chicago_geocoder_upload_mapping_$(REZONING_DATE_TAG).csv: ../output/chicago_geocoder_upload_$(REZONING_DATE_TAG).xlsx

../output/census_geocoder_upload_template_$(REZONING_DATE_TAG).csv: build_census_template.R ../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv | ../output run.sbatch
	$(R) $< --date-tag $(REZONING_DATE_TAG)

../input/zoning_matters_far_$(REZONING_DATE_TAG).csv: ../../rezoning_far_pre_geocode/output/zoning_matters_far_$(REZONING_DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv: ../../cook_parcel_addresses_download/output/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv: ../../cook_parcel_addresses_download/output/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/zoning_matters_far_$(REZONING_DATE_TAG).csv ../input/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv ../input/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv

include ../../shell_functions.make
include ../../generic.make
