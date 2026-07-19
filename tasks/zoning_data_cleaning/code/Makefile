ZONING_RAW_NAME ?= Boundaries_-_Zoning_Districts_20250910.geojson

all: ../output/zoning_data_clean.gpkg ../output/zoning_far_lookup_clean.csv

../output/zoning_data_clean.gpkg: clean_zoning_data.R ../input/raw_zoning_data.geojson ../input/zoning-code-summary-district-types.csv ../input/old_zoning_bulk_density_1957_2004.csv | ../output
	$(R) $<

../output/zoning_far_lookup_clean.csv: ../output/zoning_data_clean.gpkg
	@test -s "$@"

../input/raw_zoning_data.geojson: ../../../data_raw/$(ZONING_RAW_NAME) | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/zoning-code-summary-district-types.csv: ../../../data_raw/zoning-code-summary-district-types.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/old_zoning_bulk_density_1957_2004.csv: ../../old_zoning_data/output/old_zoning_bulk_density_1957_2004.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/raw_zoning_data.geojson ../input/zoning-code-summary-district-types.csv ../input/old_zoning_bulk_density_1957_2004.csv

include ../../shell_functions.make
include ../../generic.make
