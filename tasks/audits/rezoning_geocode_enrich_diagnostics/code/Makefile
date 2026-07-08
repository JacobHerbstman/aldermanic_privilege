SHELL=bash
include ../../../shell_functions.make
include ../../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
MAP_PYTHON ?= $(PYTHON)
REZONING_DATE_TAG ?= 19990101_20260212

all: \
	../output/ward_rezoning_counts_$(REZONING_DATE_TAG).csv \
	../output/sponsor_ward_validation_$(REZONING_DATE_TAG).csv \
	../output/map_geocode_source_$(REZONING_DATE_TAG).pdf \
	../output/map_upzone_downzone_$(REZONING_DATE_TAG).pdf

link-inputs: ../input/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv

../input/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv: ../../../rezoning_geocode_enrich/output/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv | ../input
	ln -sf "$<" "$@"

../output/ward_rezoning_counts_$(REZONING_DATE_TAG).csv: enrich_geocoded_summary.py ../../../rezoning_geocode_enrich/code/enrich_geocoded_rezonings.py ../input/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv | ../output ../temp run.sbatch
	$(PYTHON) enrich_geocoded_summary.py --date-tag $(REZONING_DATE_TAG)

../output/sponsor_ward_validation_$(REZONING_DATE_TAG).csv: ../output/ward_rezoning_counts_$(REZONING_DATE_TAG).csv

../output/map_geocode_source_$(REZONING_DATE_TAG).pdf: enrich_geocoded_map_source.py ../../../rezoning_geocode_enrich/code/enrich_geocoded_rezonings.py ../input/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv | ../output ../temp run.sbatch
	MPLCONFIGDIR=../temp/matplotlib $(MAP_PYTHON) enrich_geocoded_map_source.py --date-tag $(REZONING_DATE_TAG)

../output/map_upzone_downzone_$(REZONING_DATE_TAG).pdf: enrich_geocoded_map_upzone.py ../../../rezoning_geocode_enrich/code/enrich_geocoded_rezonings.py ../input/rezoning_geocoded_enriched_$(REZONING_DATE_TAG).csv | ../output ../temp run.sbatch
	MPLCONFIGDIR=../temp/matplotlib $(MAP_PYTHON) enrich_geocoded_map_upzone.py --date-tag $(REZONING_DATE_TAG)
