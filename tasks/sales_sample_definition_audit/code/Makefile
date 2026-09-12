include ../../shared/code/shell_functions.make

all: ../output/sales_sample_flow.csv \
	../output/sales_sample_definition_summary.csv \
	../output/sales_sample_definition_by_year.csv \
	../output/sales_sample_definition_deed_composition.csv \
	../output/sales_sample_definition_deed_quality.csv \
	../output/sales_sample_definition_overlap.csv \
	../output/sales_sample_definition_candidates.parquet \
	../output/sales_sample_definition_coordinate_keys.csv \
	../output/sales_sample_definition_historical_coordinates.csv \
	../output/sales_sample_definition_rd_estimates.csv \
	../output/sales_sample_definition_rd_counts.csv \
	../output/sales_sample_definition_spatial_coverage.csv \
	../output/sales_sample_definition_rd_estimates.pdf \
	../output/sales_sample_definition_comparison.pdf

../output/sales_sample_flow.csv: build_sales_sample_flow.R \
	../input/parcel_sales_city.csv \
	../input/residential_sales_clean.parquet \
	../input/residential_improvements_panel.parquet \
	../input/sales_pre_scores.csv \
	../input/sales_with_hedonics.parquet \
	../input/unfiltered_sales_with_hedonics_amenities.parquet \
	../input/clean_sales_with_hedonics_amenities.parquet \
	../input/price_boundary_property_type_fe_estimates.csv | ../output
	$(R) $<
../output/sales_sample_definition_summary.csv: build_sales_sample_definition_audit.R ../input/parcel_sales_city.csv ../input/residential_improvements_panel.parquet ../input/fred_cpi_cuura207sa0.csv | ../output
	$(R) $<

../output/sales_sample_definition_by_year.csv \
../output/sales_sample_definition_deed_composition.csv \
../output/sales_sample_definition_deed_quality.csv \
../output/sales_sample_definition_overlap.csv \
../output/sales_sample_definition_candidates.parquet \
../output/sales_sample_definition_coordinate_keys.csv \
../output/sales_sample_definition_comparison.pdf: ../output/sales_sample_definition_summary.csv
	@test -s "$@"

../output/sales_sample_definition_historical_coordinates.csv: download_sales_sample_definition_coordinates.R ../output/sales_sample_definition_coordinate_keys.csv | ../output
	$(R) $<

../output/sales_sample_definition_rd_estimates.csv: \
	estimate_sales_sample_definition_rd.R \
	../../shared/code/canonical_geometry_helpers.R \
	../../shared/code/amenity_distance_helpers.R \
	../output/sales_sample_definition_candidates.parquet \
	../output/sales_sample_definition_historical_coordinates.csv \
	../input/unfiltered_sales_with_hedonics_amenities.parquet \
	../input/parcel_universe_2025_city.csv \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	../input/chicago_alderman_terms.csv \
	../input/aldermen_uncertainty_scores_through2022.csv \
	../input/boundary_segments_1320ft.gpkg \
	../input/schools_2015.gpkg \
	../input/parks.gpkg \
	../input/major_streets.gpkg \
	../input/cta_stops.gpkg \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg | ../output
	$(R) $<

../output/sales_sample_definition_rd_counts.csv \
../output/sales_sample_definition_spatial_coverage.csv \
../output/sales_sample_definition_rd_estimates.pdf: ../output/sales_sample_definition_rd_estimates.csv
	@test -s "$@"

../input/parcel_sales_city.csv: ../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	ln -sf $< $@

../input/residential_sales_clean.parquet: ../../clean_residential_sales/output/residential_sales_clean.parquet | ../input
	ln -sf $< $@

../input/residential_improvements_panel.parquet: ../../process_residential_improvements_full/output/residential_improvements_panel.parquet | ../input
	ln -sf $< $@

../input/sales_pre_scores.csv: ../../calculate_sale_distances/output/sales_pre_scores.csv | ../input
	ln -sf $< $@

../input/sales_with_hedonics.parquet: ../../sales_record_quality_audit/output/sales_with_hedonics.parquet | ../input
	ln -sf $< $@

../input/fred_cpi_cuura207sa0.csv: ../../download_fred_cpi/output/fred_cpi_cuura207sa0.csv | ../input
	ln -sf $< $@

../input/unfiltered_sales_with_hedonics_amenities.parquet: ../../sales_record_quality_audit/output/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@

../input/clean_sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@

../input/price_boundary_property_type_fe_estimates.csv: ../../price_boundary_results/output/price_boundary_property_type_fe_estimates.csv | ../input
	ln -sf $< $@

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	ln -sf $< $@

../input/aldermen_uncertainty_scores_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

../input/schools_2015.gpkg: ../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	ln -sf $< $@

../input/parks.gpkg: ../../prepare_amenities_data/output/parks.gpkg | ../input
	ln -sf $< $@

../input/major_streets.gpkg: ../../prepare_amenities_data/output/major_streets.gpkg | ../input
	ln -sf $< $@

../input/cta_stops.gpkg: ../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	ln -sf $< $@

../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	ln -sf $< $@

link-inputs: \
	../input/parcel_sales_city.csv \
	../input/residential_sales_clean.parquet \
	../input/residential_improvements_panel.parquet \
	../input/sales_pre_scores.csv \
	../input/sales_with_hedonics.parquet \
	../input/fred_cpi_cuura207sa0.csv \
	../input/unfiltered_sales_with_hedonics_amenities.parquet \
	../input/clean_sales_with_hedonics_amenities.parquet \
	../input/price_boundary_property_type_fe_estimates.csv \
	../input/parcel_universe_2025_city.csv \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	../input/chicago_alderman_terms.csv \
	../input/aldermen_uncertainty_scores_through2022.csv \
	../input/boundary_segments_1320ft.gpkg \
	../input/schools_2015.gpkg \
	../input/parks.gpkg \
	../input/major_streets.gpkg \
	../input/cta_stops.gpkg \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg

include ../../shared/code/generic.make
