SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

ZONING_RAW_NAME ?= Boundaries_-_Zoning_Districts_20250910.geojson

all: ../output/zoning_data_clean.gpkg

link-inputs: ../input/raw_zoning_data.geojson ../input/zoning-code-summary-district-types.csv ../input/old_zoning_bulk_density_1957_2004.csv

../input/raw_zoning_data.geojson: ../../../data_raw/$(ZONING_RAW_NAME) | ../input
	ln -sf $< $@

../input/zoning-code-summary-district-types.csv: ../../../data_raw/zoning-code-summary-district-types.csv | ../input
	ln -sf $< $@

../input/old_zoning_bulk_density_1957_2004.csv: ../../old_zoning_data/output/old_zoning_bulk_density_1957_2004.csv | ../input
	ln -sf "$<" "$@"

../output/zoning_far_lookup_clean.csv: ../output/zoning_data_clean.gpkg

../output/zoning_data_clean.gpkg: clean_zoning_data.R ../input/raw_zoning_data.geojson ../input/zoning-code-summary-district-types.csv ../input/old_zoning_bulk_density_1957_2004.csv | ../output run.sbatch
	$(R) $< \
		--in-zoning-geojson ../input/raw_zoning_data.geojson \
		--in-zoning-lookup-csv ../input/zoning-code-summary-district-types.csv \
		--in-old-zoning-csv ../input/old_zoning_bulk_density_1957_2004.csv \
		--out-zoning-gpkg ../output/zoning_data_clean.gpkg \
		--out-far-lookup-csv ../output/zoning_far_lookup_clean.csv

test: test_clean_zoning_data.R clean_zoning_data.R smoke/raw_zoning_data.geojson smoke/zoning-code-summary-district-types.csv smoke/old_zoning_bulk_density_1957_2004.csv
	$(R) test_clean_zoning_data.R
