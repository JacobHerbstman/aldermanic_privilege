DATE_TAG ?= 20101101_20201231

all: ../output/rezoning_census_blocks_$(DATE_TAG).csv ../output/rezoning_matter_block_bridge_$(DATE_TAG).csv

../output/rezoning_census_blocks_$(DATE_TAG).csv: assign_rezonings_to_blocks.R ../input/rezoning_geocode_with_external_$(DATE_TAG).csv ../input/rezoning_parcel_matches_$(DATE_TAG).csv ../input/reviewed_rezoning_block_assignments.csv ../input/census_blocks_2010.csv | ../output run.sbatch
	$(R) $< $(DATE_TAG)

../output/rezoning_matter_block_bridge_$(DATE_TAG).csv: ../output/rezoning_census_blocks_$(DATE_TAG).csv

../input/rezoning_geocode_with_external_$(DATE_TAG).csv: ../../rezoning_geocode_external_merge/output/rezoning_geocode_with_external_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rezoning_parcel_matches_$(DATE_TAG).csv: ../../rezoning_geocode_stage1_parcel/output/rezoning_parcel_matches_$(DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../download_chicago_spatial_data/output/census_blocks_2010.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/reviewed_rezoning_block_assignments.csv: ../../rezoning_block_hand_adjudications/output/reviewed_rezoning_block_assignments.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rezoning_geocode_with_external_$(DATE_TAG).csv ../input/rezoning_parcel_matches_$(DATE_TAG).csv ../input/reviewed_rezoning_block_assignments.csv ../input/census_blocks_2010.csv

include ../../shell_functions.make
include ../../generic.make
