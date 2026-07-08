SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
MODERN_DATE_TAG ?= 20101201_20260212
HIST_YEAR_TAG ?= 1999_2010
DATASET_DATE_TAG ?= 19990101_20260212

all: harmonize

harmonize: ../output/rezoning_dataset_harmonized_$(DATASET_DATE_TAG).csv

finalize: ../output/rezoning_dataset_all_$(DATASET_DATE_TAG).csv ../output/rezoning_dataset_usable_$(DATASET_DATE_TAG).csv

both: ../output/rezoning_dataset_harmonized_$(DATASET_DATE_TAG).csv ../output/rezoning_dataset_all_$(DATASET_DATE_TAG).csv ../output/rezoning_dataset_usable_$(DATASET_DATE_TAG).csv

link-inputs: link-inputs-harmonize

link-inputs-harmonize: ../input/zoning_matters_$(MODERN_DATE_TAG).csv ../input/journal_rezonings_first_pass_$(HIST_YEAR_TAG).csv

link-inputs-finalize: ../input/rezoning_geocoded_enriched_$(DATASET_DATE_TAG).csv ../input/chicago_alderman_panel.csv

../input/zoning_matters_$(MODERN_DATE_TAG).csv: ../../build_zoning_dataset/output/zoning_matters_$(MODERN_DATE_TAG).csv | ../input
	ln -sf "$<" "$@"

../input/journal_rezonings_first_pass_$(HIST_YEAR_TAG).csv: ../../clerk_journals_first_pass_features/output/journal_rezonings_first_pass_$(HIST_YEAR_TAG).csv | ../input
	ln -sf "$<" "$@"

../input/rezoning_geocoded_enriched_$(DATASET_DATE_TAG).csv: ../../rezoning_geocode_enrich/output/rezoning_geocoded_enriched_$(DATASET_DATE_TAG).csv | ../input
	ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

../output/rezoning_dataset_harmonized_$(DATASET_DATE_TAG).csv: harmonize_rezoning_dataset.py ../input/zoning_matters_$(MODERN_DATE_TAG).csv ../input/journal_rezonings_first_pass_$(HIST_YEAR_TAG).csv | ../output run.sbatch
	$(PYTHON) harmonize_rezoning_dataset.py \
		--modern-date-tag $(MODERN_DATE_TAG) \
		--hist-year-tag $(HIST_YEAR_TAG) \
		--dataset-date-tag $(DATASET_DATE_TAG)

../output/rezoning_dataset_all_$(DATASET_DATE_TAG).csv: finalize_rezoning_dataset.py ../input/rezoning_geocoded_enriched_$(DATASET_DATE_TAG).csv ../input/chicago_alderman_panel.csv | ../output run.sbatch
	$(PYTHON) finalize_rezoning_dataset.py \
		--dataset-date-tag $(DATASET_DATE_TAG)

../output/rezoning_dataset_usable_$(DATASET_DATE_TAG).csv: ../output/rezoning_dataset_all_$(DATASET_DATE_TAG).csv

test: test_build_rezoning_dataset.py harmonize_rezoning_dataset.py finalize_rezoning_dataset.py
	$(PYTHON) -m unittest test_build_rezoning_dataset.py
