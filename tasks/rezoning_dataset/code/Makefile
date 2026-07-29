MODERN_DATE_TAG ?= 20101201_20260212
HIST_YEAR_TAG ?= 1999_2010
DATASET_DATE_TAG ?= 19990101_20260212

all: ../output/rezoning_dataset_harmonized_$(DATASET_DATE_TAG).csv

../output/rezoning_dataset_harmonized_$(DATASET_DATE_TAG).csv: harmonize_rezoning_dataset.py ../input/zoning_matters_$(MODERN_DATE_TAG).csv ../input/journal_rezonings_first_pass_$(HIST_YEAR_TAG).csv | ../output run.sbatch
	$(PYTHON) $< \
		--modern-date-tag $(MODERN_DATE_TAG) \
		--hist-year-tag $(HIST_YEAR_TAG) \
		--dataset-date-tag $(DATASET_DATE_TAG)

../output/rezoning_dataset_all_$(DATASET_DATE_TAG).csv: finalize_rezoning_dataset.py ../input/rezoning_geocoded_enriched_$(DATASET_DATE_TAG).csv ../input/chicago_alderman_panel.csv | ../output run.sbatch
	$(PYTHON) $< --dataset-date-tag $(DATASET_DATE_TAG)

../output/rezoning_dataset_usable_$(DATASET_DATE_TAG).csv: ../output/rezoning_dataset_all_$(DATASET_DATE_TAG).csv

../input/zoning_matters_$(MODERN_DATE_TAG).csv: ../../build_zoning_dataset/output/zoning_matters_$(MODERN_DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/journal_rezonings_first_pass_$(HIST_YEAR_TAG).csv: ../../clerk_journals_first_pass_features/output/journal_rezonings_first_pass_$(HIST_YEAR_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rezoning_geocoded_enriched_$(DATASET_DATE_TAG).csv: ../../rezoning_geocode_enrich/output/rezoning_geocoded_enriched_$(DATASET_DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/zoning_matters_$(MODERN_DATE_TAG).csv ../input/journal_rezonings_first_pass_$(HIST_YEAR_TAG).csv ../input/rezoning_geocoded_enriched_$(DATASET_DATE_TAG).csv ../input/chicago_alderman_panel.csv

include ../../shell_functions.make
include ../../generic.make
