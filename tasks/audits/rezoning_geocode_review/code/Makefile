DATE_TAG ?= 20101101_20201231
SLEEP_SECONDS ?= 0.1
TIMEOUT_SECONDS ?= 30

all: ../output/census_geocoder_review_$(DATE_TAG).csv

../output/census_geocoder_review_$(DATE_TAG).csv: query_census_geocoder.py ../input/rezoning_geocode_stage1_unmatched_$(DATE_TAG).csv ../input/rezoning_external_geocodes_$(DATE_TAG).csv | ../output run.sbatch
	$(PYTHON) $< $(DATE_TAG) $(SLEEP_SECONDS) $(TIMEOUT_SECONDS)

../input/rezoning_geocode_stage1_unmatched_$(DATE_TAG).csv: ../../../rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_unmatched_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rezoning_external_geocodes_$(DATE_TAG).csv: ../../../rezoning_geocode_external_merge_ready/output/rezoning_external_geocodes_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rezoning_geocode_stage1_unmatched_$(DATE_TAG).csv ../input/rezoning_external_geocodes_$(DATE_TAG).csv

include ../../../shell_functions.make
include ../../../generic.make
