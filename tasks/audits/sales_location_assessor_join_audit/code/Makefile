all: \
	../output/audit_findings.md \
	../output/sales_coordinate_match_summary.csv \
	../output/sales_coordinate_fallback_cases.csv \
	../output/sales_assessor_match_summary.csv \
	../output/sales_assessor_gap_cases.csv \
	../output/sales_rd_match_sensitivity.csv \
	../output/historical_parcel_records.csv \
	../output/historical_parcel_match_summary.csv \
	../output/historical_parcel_match_cases.csv \
	../output/historical_sales_rd_sensitivity.csv

../output/audit_findings.md: write_audit_findings.R ../output/sales_coordinate_match_summary.csv ../output/sales_assessor_match_summary.csv ../output/sales_rd_match_sensitivity.csv ../output/historical_parcel_match_summary.csv ../output/historical_sales_rd_sensitivity.csv | ../output
	Rscript $<

../output/sales_coordinate_match_summary.csv: run_sales_location_assessor_join_audit.R ../input/parcel_sales.csv ../input/parcel_universe.csv ../input/ward_panel.gpkg ../input/residential_improvements_panel.parquet ../input/sales_with_hedonics_amenities.parquet | ../output
	Rscript $<

../output/sales_coordinate_fallback_cases.csv \
../output/sales_assessor_match_summary.csv \
../output/sales_assessor_gap_cases.csv \
../output/sales_rd_match_sensitivity.csv: ../output/sales_coordinate_match_summary.csv
	@test -s "$@"

../output/historical_parcel_records.csv: fetch_historical_parcel_records.R ../output/sales_coordinate_fallback_cases.csv | ../output
	Rscript $<

../output/historical_parcel_match_summary.csv: analyze_historical_parcel_matches.R ../../../_lib/canonical_geometry_helpers.R ../output/sales_coordinate_fallback_cases.csv ../output/historical_parcel_records.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores_through2022.csv | ../output
	Rscript $<

../output/historical_parcel_match_cases.csv: ../output/historical_parcel_match_summary.csv
	@test -s "$@"

../output/historical_sales_rd_sensitivity.csv: reestimate_with_historical_sales.R ../../../_lib/amenity_distance_helpers.R ../output/historical_parcel_match_cases.csv ../input/parcel_sales.csv ../input/fred_cpi_cuura207sa0.csv ../input/residential_improvements_panel.parquet ../input/sales_with_hedonics_amenities.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript $<

../input/parcel_sales.csv: ../../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_universe.csv: ../../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/fred_cpi_cuura207sa0.csv: ../../../download_fred_cpi/output/fred_cpi_cuura207sa0.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../../prepare_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../../prepare_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.%: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/residential_improvements_panel.parquet: ../../../process_residential_improvements_full/output/residential_improvements_panel.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcel_sales.csv ../input/parcel_universe.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_1320ft.gpkg ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores_through2022.csv ../input/fred_cpi_cuura207sa0.csv ../input/residential_improvements_panel.parquet ../input/sales_with_hedonics_amenities.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../../generic.make
