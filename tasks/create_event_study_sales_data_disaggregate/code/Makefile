SHELL := bash
include ../../shell_functions.make

all: \
	../output/sales_transaction_panel.parquet \
	../output/sales_transaction_panel_announcement.parquet \
	../output/sales_transaction_panel_2012.parquet \
	../output/sales_transaction_panel_2015.parquet \
	../output/sales_transaction_panel_2022.parquet \
	../output/sales_transaction_panel_2023.parquet \
	../output/sales_transaction_panel_amenity_distance_diagnostics.csv \
	../output/sales_transaction_panel_support_by_event_time.csv \
	../output/sales_transaction_panel_support_by_calendar_time.csv \
	../output/sales_transaction_panel_assignment_stability.csv

../output/sales_transaction_panel.parquet: create_transaction_panel.R ../../_lib/canonical_geometry_helpers.R ../../_lib/amenity_distance_helpers.R ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/boundary_segments_1320ft.gpkg ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	$(R) $<

../output/sales_transaction_panel_announcement.parquet \
../output/sales_transaction_panel_2012.parquet \
../output/sales_transaction_panel_2015.parquet \
../output/sales_transaction_panel_2022.parquet \
../output/sales_transaction_panel_2023.parquet \
../output/sales_transaction_panel_amenity_distance_diagnostics.csv \
../output/sales_transaction_panel_support_by_event_time.csv \
../output/sales_transaction_panel_support_by_calendar_time.csv \
../output/sales_transaction_panel_assignment_stability.csv: ../output/sales_transaction_panel.parquet
	test -f $@

../input/sales_with_ward_distances.csv: ../../merge_event_study_scores/output/sales_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/residential_improvements_panel.parquet: ../../process_residential_improvements_full/output/residential_improvements_panel.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_treatment_panel.csv: ../../merge_event_study_scores/output/block_treatment_panel.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2020.csv: ../../census_block_2020_cleaning/output/census_blocks_2020.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../download_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../download_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../download_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.dbf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.prj | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shx | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.cpg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv ../input/census_blocks_2020.csv ../input/boundary_segments_1320ft.gpkg ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg
