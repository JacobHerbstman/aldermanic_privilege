PERMIT_START_YEAR ?= 2010
PERMIT_END_MONTH ?= 2020-12

all: ../output/unmatched_permit_block_review.csv

../output/unmatched_permit_block_review.csv: review_unmatched_permits.R ../input/building_permits_clean.gpkg ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv | ../output run.sbatch
	$(R) $< $(PERMIT_START_YEAR) $(PERMIT_END_MONTH)

../input/building_permits_clean.gpkg: ../../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../download_chicago_spatial_data/output/census_blocks_2010.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2020.csv: ../../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/building_permits_clean.gpkg ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv

include ../../../shell_functions.make
include ../../../generic.make
