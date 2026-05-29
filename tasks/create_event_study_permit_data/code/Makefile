SEGMENT_LENGTH_FT ?= 1320
SEGMENT_BUFFER_M ?= 304.8
PANEL_MAX_DISTANCE_M ?= 800
PERMIT_START_YEAR ?= 2010
PERMIT_END_YEAR ?= 2020
PERMIT_END_MONTH ?= 2020-12

all: \
	../output/permit_block_year_panel.parquet \
	../output/permit_block_year_panel_2015.parquet \
	../output/permit_block_year_panel_2023.parquet

../output/permit_block_year_panel.parquet: create_permit_block_year_panel.R ../../_lib/canonical_geometry_helpers.R ../input/building_permits_clean.gpkg ../input/building_permits_text_features.csv.gz ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/ward_panel.gpkg ../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg | ../output
	Rscript $< $(SEGMENT_BUFFER_M) $(PANEL_MAX_DISTANCE_M) $(PERMIT_START_YEAR) $(PERMIT_END_YEAR) $(PERMIT_END_MONTH)

../output/permit_block_year_panel_2015.parquet \
../output/permit_block_year_panel_2023.parquet: ../output/permit_block_year_panel.parquet
	@test -s "$@"

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/building_permits_text_features.csv.gz: ../../building_permits_scraping/output/building_permits_text_features.csv.gz | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_treatment_panel.csv: ../../merge_event_study_scores/output/block_treatment_panel.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2020.csv: ../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg: ../../border_segment_creation/output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/building_permits_clean.gpkg ../input/building_permits_text_features.csv.gz ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/ward_panel.gpkg ../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg

include ../../generic.make
