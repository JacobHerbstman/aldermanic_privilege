MIN_OVERLAP_SQFT ?= 100

all: ../output/reviewed_rezoning_block_assignments.csv

../output/reviewed_rezoning_block_assignments.csv: build_reviewed_block_assignments.R ../input/zoning_districts_2025.geojson ../input/census_blocks_2010.csv | ../output
	$(R) $< $(MIN_OVERLAP_SQFT)

../input/zoning_districts_2025.geojson: ../../../../data_raw/Boundaries_-_Zoning_Districts_20250910.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../download_chicago_spatial_data/output/census_blocks_2010.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/zoning_districts_2025.geojson ../input/census_blocks_2010.csv

include ../../../shell_functions.make
include ../../../generic.make
