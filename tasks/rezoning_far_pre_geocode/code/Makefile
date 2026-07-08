SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
DATE_TAG ?= 19990101_20260212

all: ../output/zoning_matters_far_$(DATE_TAG).csv ../output/far_unresolved_rows_$(DATE_TAG).csv ../output/rezoning_far_pre_geocode_summary_$(DATE_TAG).json

link-inputs: ../input/rezoning_dataset_harmonized_$(DATE_TAG).csv ../input/zoning_far_lookup_clean.csv

../input/rezoning_dataset_harmonized_$(DATE_TAG).csv: ../../rezoning_dataset/output/rezoning_dataset_harmonized_$(DATE_TAG).csv | ../input
	ln -sf "$<" "$@"

../input/zoning_far_lookup_clean.csv: ../../zoning_data_cleaning/output/zoning_far_lookup_clean.csv | ../input
	ln -sf "$<" "$@"

../output/zoning_matters_far_$(DATE_TAG).csv: assign_far_pre_geocode.py ../input/rezoning_dataset_harmonized_$(DATE_TAG).csv ../input/zoning_far_lookup_clean.csv | ../output ../temp run.sbatch
	$(PYTHON) assign_far_pre_geocode.py \
		--in-master-csv ../input/rezoning_dataset_harmonized_$(DATE_TAG).csv \
		--in-zoning-lookup-csv ../input/zoning_far_lookup_clean.csv \
		--out-far-csv ../output/zoning_matters_far_$(DATE_TAG).csv \
		--out-unresolved-csv ../output/far_unresolved_rows_$(DATE_TAG).csv

../output/far_unresolved_rows_$(DATE_TAG).csv: ../output/zoning_matters_far_$(DATE_TAG).csv

../output/rezoning_far_pre_geocode_summary_$(DATE_TAG).json: summarize_far_pre_geocode.R ../output/zoning_matters_far_$(DATE_TAG).csv ../output/far_unresolved_rows_$(DATE_TAG).csv | ../output run.sbatch
	$(R) summarize_far_pre_geocode.R \
		--date-tag $(DATE_TAG)

test: test_assign_far_pre_geocode.py assign_far_pre_geocode.py summarize_far_pre_geocode.R
	$(PYTHON) -m unittest test_assign_far_pre_geocode.py
