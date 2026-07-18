all: ../output/sales_data_cleaning_audit.csv

../output/sales_data_cleaning_audit.csv: audit_sales_data_cleaning.R ../../../create_alderman_data/code/create_alderman_panel.R ../input/parcel_sales_city.csv ../input/sales_pre_scores_with_segments.csv ../input/sales_with_hedonics_amenities.parquet ../input/aldermen_uncertainty_scores_through2022.csv ../input/cta_stops.gpkg | ../output
	Rscript $<

../output/sales_model_historical_coordinates.csv: fetch_sales_model_historical_coordinates.R ../input/sales_with_hedonics_amenities.parquet | ../output
	Rscript $<

../output/sales_historical_coordinate_check.csv: audit_sales_historical_coordinates.R ../../../_lib/canonical_geometry_helpers.R ../../../_lib/amenity_distance_helpers.R ../output/sales_model_historical_coordinates.csv ../input/sales_with_hedonics_amenities.parquet ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores_through2022.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript $<

../output/sales_historical_coordinate_examples.csv: ../output/sales_historical_coordinate_check.csv
	@test -s "$@"

../input/parcel_sales_city.csv: ../../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_pre_scores_with_segments.csv: ../../../assign_segment_ids_sales_rental/output/sales_pre_scores_with_segments.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cta_stops.gpkg: ../../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../../prepare_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../../prepare_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.%: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/parcel_sales_city.csv \
	../input/sales_pre_scores_with_segments.csv \
	../input/sales_with_hedonics_amenities.parquet \
	../input/aldermen_uncertainty_scores_through2022.csv \
	../input/cta_stops.gpkg \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	../input/boundary_segments_1320ft.gpkg \
	../input/chicago_alderman_panel.csv \
	../input/schools_2015.gpkg \
	../input/parks.gpkg \
	../input/major_streets.gpkg \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg

include ../../../generic.make
