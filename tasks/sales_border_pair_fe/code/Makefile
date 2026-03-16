SHELL := bash
include ../../shell_functions.make

FE_GEOS := segment ward_pair
CLUSTERS := segment ward_pair
ROBUST_FE_OUTPUTS := $(foreach geo,$(FE_GEOS),$(foreach cl,$(CLUSTERS),$(if $(filter segment,$(geo)),$(if $(filter segment,$(cl)),,../output/fe_table_sales_bw1000_year_quarter_geo_$(geo)_clust_$(cl).tex),../output/fe_table_sales_bw1000_year_quarter_geo_$(geo)_clust_$(cl).tex)))
SLIDE_WARD_PAIR_OUTPUTS := \
	../output/sales_with_hedonics_amenities.parquet \
	../output/sales_amenity_distance_diagnostics.csv \
	../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex \
	../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.csv

all: $(SLIDE_WARD_PAIR_OUTPUTS) ../output/fe_table_sales_bw1000_year_quarter.tex $(ROBUST_FE_OUTPUTS)

../output/fe_table_sales_bw1000_year_quarter.tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet | ../output
	Rscript sales_border_pair_fe.R ../input/sales_with_hedonics.parquet 1000 year_quarter "$@" ../output/fe_table_sales_bw1000_year_quarter.csv ../output/year_diagnostics_sales_bw1000_year_quarter.csv segment segment

../output/fe_table_sales_bw1000_year_quarter_zonegroup.tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet ../input/zoning_data_clean.gpkg | ../output
	USE_ZONE_GROUP_FE=TRUE ZONING_GPKG=../input/zoning_data_clean.gpkg Rscript sales_border_pair_fe.R ../input/sales_with_hedonics.parquet 1000 year_quarter "$@" ../output/fe_table_sales_bw1000_year_quarter_zonegroup.csv ../output/year_diagnostics_sales_bw1000_year_quarter_zonegroup.csv segment segment

../output/fe_table_sales_bw500_year_quarter.tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet | ../output
	Rscript sales_border_pair_fe.R ../input/sales_with_hedonics.parquet 500 year_quarter "$@" ../output/fe_table_sales_bw500_year_quarter.csv ../output/year_diagnostics_sales_bw500_year_quarter.csv segment segment

../output/fe_table_sales_bw500_year_quarter_clust_ward_pair.tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet | ../output
	Rscript sales_border_pair_fe.R ../input/sales_with_hedonics.parquet 500 year_quarter "$@" ../output/fe_table_sales_bw500_year_quarter_clust_ward_pair.csv ../output/year_diagnostics_sales_bw500_year_quarter_clust_ward_pair.csv segment ward_pair

../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex: sales_border_pair_fe.R ../output/sales_with_hedonics_amenities.parquet | ../output
	Rscript sales_border_pair_fe.R ../output/sales_with_hedonics_amenities.parquet 500 year_quarter "$@" ../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.csv ../output/year_diagnostics_sales_bw500_year_quarter_amenity_clust_ward_pair.csv segment ward_pair amenity

../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.csv: ../output/fe_table_sales_bw500_year_quarter_amenity_clust_ward_pair.tex
	test -f "$@"

../output/fe_table_sales_bw500_year_quarter_zonegroup.tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet ../input/zoning_data_clean.gpkg | ../output
	USE_ZONE_GROUP_FE=TRUE ZONING_GPKG=../input/zoning_data_clean.gpkg Rscript sales_border_pair_fe.R ../input/sales_with_hedonics.parquet 500 year_quarter "$@" ../output/fe_table_sales_bw500_year_quarter_zonegroup.csv ../output/year_diagnostics_sales_bw500_year_quarter_zonegroup.csv segment segment

../output/fe_table_sales_bw500_year_quarter_pruned.tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet ../input/confounded_pair_era_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv Rscript sales_border_pair_fe.R ../input/sales_with_hedonics.parquet 500 year_quarter "$@" ../output/fe_table_sales_bw500_year_quarter_pruned.csv ../output/year_diagnostics_sales_bw500_year_quarter_pruned.csv segment segment

../output/fe_table_sales_bw500_year_quarter_pruned_zonegroup.tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet ../input/confounded_pair_era_flags.csv ../input/zoning_data_clean.gpkg | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv USE_ZONE_GROUP_FE=TRUE ZONING_GPKG=../input/zoning_data_clean.gpkg Rscript sales_border_pair_fe.R ../input/sales_with_hedonics.parquet 500 year_quarter "$@" ../output/fe_table_sales_bw500_year_quarter_pruned_zonegroup.csv ../output/year_diagnostics_sales_bw500_year_quarter_pruned_zonegroup.csv segment segment

define FE_ROBUST_RULE
../output/fe_table_sales_bw1000_year_quarter_geo_$(1)_clust_$(2).tex: sales_border_pair_fe.R ../input/sales_with_hedonics.parquet | ../output
	Rscript sales_border_pair_fe.R ../input/sales_with_hedonics.parquet 1000 year_quarter $$@ ../output/fe_table_sales_bw1000_year_quarter_geo_$(1)_clust_$(2).csv ../output/year_diagnostics_sales_bw1000_year_quarter_geo_$(1)_clust_$(2).csv $(1) $(2)
endef
$(foreach geo,$(FE_GEOS),$(foreach cl,$(CLUSTERS),$(if $(and $(filter segment,$(geo)),$(filter segment,$(cl))),,$(eval $(call FE_ROBUST_RULE,$(geo),$(cl))))))

../output/sales_with_hedonics_amenities.parquet: enrich_sales_with_amenity_distances.R ../input/sales_with_hedonics.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript enrich_sales_with_amenity_distances.R ../input/sales_with_hedonics.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp "$@" ../output/sales_amenity_distance_diagnostics.csv

../output/sales_amenity_distance_diagnostics.csv: ../output/sales_with_hedonics_amenities.parquet
	test -f "$@"

../input/sales_with_hedonics.parquet: ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

../input/zoning_data_clean.gpkg: ../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
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

link-inputs: ../input/sales_with_hedonics.parquet ../input/confounded_pair_era_flags.csv ../input/zoning_data_clean.gpkg ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../generic.make
