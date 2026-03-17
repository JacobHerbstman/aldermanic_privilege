SHELL := bash
include ../../shell_functions.make

all: \
	../output/sales_with_hedonics_amenities.parquet \
	../output/sales_amenity_distance_diagnostics.csv \
	../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex \
	../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.csv \
	../output/year_diagnostics_sales_bw500_year_quarter_amenity_clust_ward_pair.csv

../output/sales_with_hedonics_amenities.parquet: enrich_sales_with_amenity_distances.R ../input/sales_with_hedonics.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript enrich_sales_with_amenity_distances.R ../input/sales_with_hedonics.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp "$@" ../output/sales_amenity_distance_diagnostics.csv

../output/sales_amenity_distance_diagnostics.csv: ../output/sales_with_hedonics_amenities.parquet
	test -f "$@"

../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex: sales_border_pair_fe.R ../output/sales_with_hedonics_amenities.parquet | ../output
	Rscript sales_border_pair_fe.R ../output/sales_with_hedonics_amenities.parquet 500 year_quarter "$@" ../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.csv ../output/year_diagnostics_sales_bw500_year_quarter_amenity_clust_ward_pair.csv segment ward_pair amenity

../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.csv: ../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex
	test -f "$@"

../output/year_diagnostics_sales_bw500_year_quarter_amenity_clust_ward_pair.csv: ../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex
	test -f "$@"

../input/sales_with_hedonics.parquet: ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../download_amenities_data/output/schools_2015.gpkg | ../input
	ln -sf "$<" "$@"

../input/parks.gpkg: ../../download_amenities_data/output/parks.gpkg | ../input
	ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../download_amenities_data/output/major_streets.gpkg | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.dbf | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.prj | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shx | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.cpg | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/sales_with_hedonics.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../generic.make
