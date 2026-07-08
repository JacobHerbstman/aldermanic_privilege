SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

DATE_TAG ?= 19990101_20260212

all: \
	../output/chicago_geocoder_results_$(DATE_TAG).csv \
	../output/chicago_geocoder_results_$(DATE_TAG).xlsx \
	../output/chicago_geocoder_results_$(DATE_TAG)_import_meta.json \
	../output/census_geocoder_results_$(DATE_TAG).csv \
	../output/census_geocoder_queries_$(DATE_TAG).csv

../output/chicago_geocoder_results_$(DATE_TAG).csv:
	@test -f "$@" || { \
		echo "Missing committed merge-ready Chicago geocoder CSV: $@"; \
		exit 1; \
	}

../output/chicago_geocoder_results_$(DATE_TAG).xlsx:
	@test -f "$@" || { \
		echo "Missing committed merge-ready Chicago geocoder workbook: $@"; \
		exit 1; \
	}

../output/chicago_geocoder_results_$(DATE_TAG)_import_meta.json:
	@test -f "$@" || { \
		echo "Missing committed merge-ready Chicago geocoder import metadata: $@"; \
		exit 1; \
	}

../output/census_geocoder_results_$(DATE_TAG).csv:
	@test -f "$@" || { \
		echo "Missing committed merge-ready Census geocoder CSV: $@"; \
		exit 1; \
	}

../output/census_geocoder_queries_$(DATE_TAG).csv:
	@test -f "$@" || { \
		echo "Missing committed merge-ready Census geocoder query audit: $@"; \
		exit 1; \
	}
