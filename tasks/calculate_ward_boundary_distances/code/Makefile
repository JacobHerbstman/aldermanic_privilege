MAX_CONSTRUCTION_YEAR ?= 2026
MAX_CONSTRUCTION_MONTH ?= 2026-04

all: ../output/parcels_pre_scores.csv ../output/parcels_with_geometry.gpkg

../output/parcels_pre_scores.csv: calculate_distances.R ../../_lib/canonical_geometry_helpers.R ../input/geocoded_residential_data.gpkg ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/block_group_controls.csv ../input/block_group_geometry_2019.gpkg ../input/zoning_data_clean.gpkg | ../output
	MAX_CONSTRUCTION_YEAR=$(MAX_CONSTRUCTION_YEAR) MAX_CONSTRUCTION_MONTH=$(MAX_CONSTRUCTION_MONTH) Rscript $<

../output/parcels_with_geometry.gpkg: ../output/parcels_pre_scores.csv
	@test -s "$@"

../input/geocoded_residential_data.gpkg: ../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_controls.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_group_geometry_2019.gpkg: ../../create_block_group_controls/output/block_group_geometry_2019.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/zoning_data_clean.gpkg: ../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"


link-inputs: ../input/geocoded_residential_data.gpkg ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/block_group_controls.csv ../input/block_group_geometry_2019.gpkg ../input/zoning_data_clean.gpkg

include ../../generic.make
