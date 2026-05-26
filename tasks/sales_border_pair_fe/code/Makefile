SHELL := bash

all: \
	../output/sales_with_hedonics_amenities.parquet \
	../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex \
	../output/sales_rd_flat_bw500_year_quarter_amenity_clust_segment.pdf

../output/sales_with_hedonics_amenities.parquet: enrich_sales_with_amenity_distances.R ../input/sales_with_hedonics.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript enrich_sales_with_amenity_distances.R

../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex: sales_border_pair_fe.R ../output/sales_with_hedonics_amenities.parquet | ../output
	Rscript sales_border_pair_fe.R 500 year_quarter segment ward_pair amenity strictness_score

../output/fe_table_sales_right_bw500_year_quarter_amenity_clust_ward_pair.tex: sales_border_pair_fe.R ../output/sales_with_hedonics_amenities.parquet | ../output
	Rscript sales_border_pair_fe.R 500 year_quarter segment ward_pair amenity right

../output/fe_table_sales_right_bw500_year_quarter_amenity_clust_segment.tex: sales_border_pair_fe.R ../output/sales_with_hedonics_amenities.parquet | ../output
	Rscript sales_border_pair_fe.R 500 year_quarter segment segment amenity right

../output/sales_rd_flat_bw500_year_quarter_amenity_clust_ward_pair.pdf: sales_level_plot.R ../output/sales_with_hedonics_amenities.parquet | ../output
	$(R) $< 500 TRUE 10 0 segment ward_pair amenity

../output/sales_rd_flat_bw500_year_quarter_amenity_clust_segment.pdf: sales_level_plot.R ../output/sales_with_hedonics_amenities.parquet | ../output
	$(R) $< 500 TRUE 10 0 segment segment amenity

../input/sales_with_hedonics.parquet: ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../prepare_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../prepare_amenities_data/output/major_streets.gpkg | ../input
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

link-inputs: ../input/sales_with_hedonics.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../shell_functions.make
include ../../generic.make
