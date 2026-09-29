include ../../shared/code/shell_functions.make

all: ../output/land_sales.csv ../output/land_boundary_estimates.csv ../output/land_remap_estimates.csv \
	../output/land_remap_sales_counts.csv ../output/land_boundary_placebo_lines.png \
	../output/teardown_redevelopment_by_side.csv

../output/land_sales.csv: build_land_sales.R ../input/parcel_sales_city.csv ../input/parcel_centroids.csv \
	../input/parcel_lot_areas_2021.csv ../input/historical_zoning_2006_candidate.gpkg ../input/zoning_nov2012.zip \
	../input/zoning_sep2014.zip ../input/zoning_jan2016.zip ../input/zoning_sep2025.geojson ../input/Wards_2014.geojson \
	../input/Wards_2015.geojson ../input/alderman_terms.csv ../input/building_permits_2006_2022.csv | ../output ../report
	$(R) $<

../output/land_boundary_estimates.csv: estimate_land_boundary.R ../output/land_sales.csv \
	../input/alderman_measures.csv ../input/stall_rate_away_from_boundaries.csv | ../output ../report
	$(R) $<

../output/teardown_redevelopment_by_side.csv: estimate_teardown_redevelopment.R ../output/land_sales.csv \
	../input/alderman_measures.csv ../input/stall_rate_away_from_boundaries.csv | ../output ../report
	$(R) $<

../output/land_boundary_placebo_lines.png: plot_land_boundary.R ../output/land_boundary_estimates.csv | ../output
	$(R) $<

../%/land_remap_estimates.csv ../%/land_remap_sales_counts.csv: estimate_land_remap.R ../output/land_sales.csv \
	../input/permit_block_year_panel_2015.parquet ../input/census_blocks_2010.csv | ../output ../report
	$(R) $<

../input/building_permits_2006_2022.csv: ../../download_building_permits/output/building_permits_2006_2022.csv | ../input
	ln -sf $< $@
../input/parcel_sales_city.csv: ../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	ln -sf $< $@
../input/parcel_centroids.csv: ../../download_parcel_centroids/output/parcel_centroids.csv | ../input
	ln -sf $< $@
../input/parcel_lot_areas_2021.csv: ../../download_parcel_polygons_2021/output/parcel_lot_areas_2021.csv | ../input
	ln -sf $< $@
../input/historical_zoning_2006_candidate.gpkg: ../../construction_zoning_history/output/historical_zoning_2006_candidate.gpkg | ../input
	ln -sf $< $@
../input/zoning_nov2012.zip: ../../../data_raw/construction_review/zoning_nov2012.zip | ../input
	ln -sf $< $@
../input/zoning_sep2014.zip: ../../../data_raw/construction_review/zoning_sep2014.zip | ../input
	ln -sf $< $@
../input/zoning_jan2016.zip: ../../../data_raw/construction_review/zoning_jan2016.zip | ../input
	ln -sf $< $@
../input/zoning_sep2025.geojson: ../../../data_raw/construction_review/zoning_sep2025.geojson | ../input
	ln -sf $< $@
../input/Wards_2014.geojson: ../../../data_raw/Wards_2014.geojson | ../input
	ln -sf $< $@
../input/Wards_2015.geojson: ../../../data_raw/Wards_2015.geojson | ../input
	ln -sf $< $@
../input/alderman_terms.csv: ../../create_alderman_data/adjudication/alderman_terms.csv | ../input
	ln -sf $< $@
../input/alderman_measures.csv: ../../explore_alderman_measures/output/alderman_measures.csv | ../input
	ln -sf $< $@
../input/stall_rate_away_from_boundaries.csv: ../../explore_alderman_measures/output/stall_rate_away_from_boundaries.csv | ../input
	ln -sf $< $@

../input/permit_block_year_panel_2015.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	ln -sf $< $@
../input/census_blocks_2010.csv: ../../download_chicago_spatial_data/output/census_blocks_2010.csv | ../input
	ln -sf $< $@

link-inputs: ../input/building_permits_2006_2022.csv ../input/permit_block_year_panel_2015.parquet \
	../input/census_blocks_2010.csv ../input/parcel_sales_city.csv ../input/parcel_centroids.csv ../input/parcel_lot_areas_2021.csv \
	../input/historical_zoning_2006_candidate.gpkg ../input/zoning_nov2012.zip ../input/zoning_sep2014.zip \
	../input/zoning_jan2016.zip ../input/zoning_sep2025.geojson ../input/Wards_2014.geojson ../input/Wards_2015.geojson \
	../input/alderman_terms.csv ../input/alderman_measures.csv ../input/stall_rate_away_from_boundaries.csv

include ../../shared/code/generic.make
