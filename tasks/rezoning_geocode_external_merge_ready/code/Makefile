DATE_TAG ?= 19990101_20260212

all: ../output/chicago_geocoder_results_$(DATE_TAG).csv ../output/chicago_geocoder_results_$(DATE_TAG).xlsx ../output/census_geocoder_results_$(DATE_TAG).csv

../output/chicago_geocoder_results_%.csv:
	@test -f "$@" || { echo "Missing committed Chicago geocoder results: $@"; exit 1; }

../output/chicago_geocoder_results_%.xlsx:
	@test -f "$@" || { echo "Missing committed Chicago geocoder workbook: $@"; exit 1; }

../output/census_geocoder_results_%.csv:
	@test -f "$@" || { echo "Missing committed Census geocoder results: $@"; exit 1; }

link-inputs:
