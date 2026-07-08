SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
MAP_PYTHON ?= $(PYTHON)
REZONING_DATE_TAG ?= 19990101_20260212
PD_FAR_REVIEW_START_YEAR ?= 2011
PD_FAR_REVIEW_END_YEAR ?= 2020
PD_FAR_REVIEW_TAG := $(PD_FAR_REVIEW_START_YEAR)_$(PD_FAR_REVIEW_END_YEAR)

all: \
	../output/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv \
	../output/ward_rezoning_counts_$(REZONING_DATE_TAG).csv \
	../output/sponsor_ward_validation_$(REZONING_DATE_TAG).csv \
	../output/map_geocode_source_$(REZONING_DATE_TAG).pdf \
	../output/map_upzone_downzone_$(REZONING_DATE_TAG).pdf

link-inputs: ../input/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv ../input/ward_panel.gpkg ../input/zoning_far_lookup_clean.csv ../input/pd_far_text_review_updates_$(PD_FAR_REVIEW_TAG).csv

../input/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv: ../../rezoning_geocode_external_merge/output/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/zoning_far_lookup_clean.csv: ../../zoning_data_cleaning/output/zoning_far_lookup_clean.csv | ../input
	ln -sf "$<" "$@"

../input/pd_far_text_review_updates_$(PD_FAR_REVIEW_TAG).csv: ../../rezoning_pd_far_merge_ready/output/merge_ready/pd_far_text_review_updates_$(PD_FAR_REVIEW_TAG).csv | ../input
	@target="$$(readlink "$@" 2>/dev/null || true)"; \
	if [ "$$target" != "$<" ]; then ln -sf "$<" "$@"; fi

../output/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv: enrich_geocoded_core.py enrich_geocoded_rezonings.py ../input/rezoning_geocode_with_external_$(REZONING_DATE_TAG).csv ../input/ward_panel.gpkg ../input/zoning_far_lookup_clean.csv ../input/pd_far_text_review_updates_$(PD_FAR_REVIEW_TAG).csv | ../output ../temp run.sbatch
	$(PYTHON) enrich_geocoded_core.py \
		--date-tag $(REZONING_DATE_TAG) \
		--pd-far-review-start-year $(PD_FAR_REVIEW_START_YEAR) \
		--pd-far-review-end-year $(PD_FAR_REVIEW_END_YEAR)

../output/ward_rezoning_counts_$(REZONING_DATE_TAG).csv: enrich_geocoded_summary.py enrich_geocoded_rezonings.py ../output/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv | ../output ../temp run.sbatch
	$(PYTHON) enrich_geocoded_summary.py --date-tag $(REZONING_DATE_TAG)

../output/sponsor_ward_validation_$(REZONING_DATE_TAG).csv: ../output/ward_rezoning_counts_$(REZONING_DATE_TAG).csv

../output/map_geocode_source_$(REZONING_DATE_TAG).pdf: enrich_geocoded_map_source.py enrich_geocoded_rezonings.py ../output/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv | ../output ../temp run.sbatch
	MPLCONFIGDIR=../temp/matplotlib $(MAP_PYTHON) enrich_geocoded_map_source.py --date-tag $(REZONING_DATE_TAG)

../output/map_upzone_downzone_$(REZONING_DATE_TAG).pdf: enrich_geocoded_map_upzone.py enrich_geocoded_rezonings.py ../output/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv | ../output ../temp run.sbatch
	MPLCONFIGDIR=../temp/matplotlib $(MAP_PYTHON) enrich_geocoded_map_upzone.py --date-tag $(REZONING_DATE_TAG)

test: test_enrich_geocoded_rezonings.py enrich_geocoded_rezonings.py
	$(PYTHON) -m unittest test_enrich_geocoded_rezonings.py
