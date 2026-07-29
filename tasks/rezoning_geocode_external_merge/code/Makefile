REZONING_DATE_TAG ?= 20101101_20201231

all: ../output/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv

../output/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv: merge_external_geocoders.R ../input/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv ../input/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv ../input/rezoning_external_geocodes_$(REZONING_DATE_TAG).csv | ../output run.sbatch
	$(R) $< $(REZONING_DATE_TAG)

../input/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv: ../../rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv: ../../rezoning_geocode_stage1_parcel/output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rezoning_external_geocodes_$(REZONING_DATE_TAG).csv: ../../rezoning_geocode_external_merge_ready/output/rezoning_external_geocodes_$(REZONING_DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv ../input/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv ../input/rezoning_external_geocodes_$(REZONING_DATE_TAG).csv

include ../../shell_functions.make
include ../../generic.make
