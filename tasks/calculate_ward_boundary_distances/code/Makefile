SHELL := bash
include ../../shell_functions.make

all: ../output/parcels_pre_scores.csv ../output/parcels_with_geometry.gpkg ../output/parcel_geometry_diagnostics.csv

../output/parcels_pre_scores.csv: calculate_distances.R ../../_lib/canonical_geometry_helpers.R ../input/geocoded_residential_data.gpkg ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/block_group_controls.csv ../input/block_group_geometry_2019.gpkg ../input/zoning_data_clean.gpkg | ../output
	$(R) calculate_distances.R

../output/parcels_with_geometry.gpkg: ../output/parcels_pre_scores.csv
	test -f ../output/parcels_with_geometry.gpkg

../output/parcel_geometry_diagnostics.csv: ../output/parcels_pre_scores.csv
	test -f ../output/parcel_geometry_diagnostics.csv

../input/geocoded_residential_data.gpkg: ../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/ward_controls.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	ln -sf $< $@

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	ln -sf $< $@

../input/block_group_geometry_2019.gpkg: ../../create_block_group_controls/output/block_group_geometry_2019.gpkg | ../input
	ln -sf $< $@

../input/zoning_data_clean.gpkg: ../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/geocoded_residential_data.gpkg ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/block_group_controls.csv ../input/block_group_geometry_2019.gpkg ../input/zoning_data_clean.gpkg
