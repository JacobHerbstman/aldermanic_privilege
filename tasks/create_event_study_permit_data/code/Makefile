PANEL_MAX_DISTANCE_M ?= 800
PERMIT_START_YEAR ?= 2010
PERMIT_END_YEAR ?= 2020

all: ../output/permit_block_year_panel_2015.parquet

../output/permit_block_year_panel_2015.parquet: create_permit_block_year_panel.R ../../_lib/canonical_geometry_helpers.R ../input/building_permits_clean.gpkg ../input/block_treatment_panel_frozen2014.csv ../input/census_blocks_2010.csv ../input/manual_permit_block_assignments.csv ../input/ward_panel.gpkg | ../output
	Rscript $< $(PANEL_MAX_DISTANCE_M) $(PERMIT_START_YEAR) $(PERMIT_END_YEAR)

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/block_treatment_panel_frozen2014.csv: ../../merge_event_study_scores/output/block_treatment_panel_frozen2014.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../download_chicago_spatial_data/output/census_blocks_2010.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/manual_permit_block_assignments.csv: ../../permit_block_hand_adjudications/output/manual_permit_block_assignments.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/building_permits_clean.gpkg ../input/block_treatment_panel_frozen2014.csv ../input/census_blocks_2010.csv ../input/manual_permit_block_assignments.csv ../input/ward_panel.gpkg

include ../../generic.make
