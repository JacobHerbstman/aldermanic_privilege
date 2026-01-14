SHELL := bash
include ../../shell_functions.make

all: ../output/zoning_data_clean.gpkg

../output/zoning_data_clean.gpkg: clean_zoning_data.R ../input/raw_zoning_data.geojson ../input/zoning-code-summary-district-types.csv | ../output
	$(R) clean_zoning_data.R

../input/raw_zoning_data.geojson: ../../../data_raw/Boundaries_-_Zoning_Districts_20250910.geojson | ../input
	ln -sf $< $@

../input/zoning-code-summary-district-types.csv: ../../../data_raw/zoning-code-summary-district-types.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/raw_zoning_data.geojson ../input/zoning-code-summary-district-types.csv
