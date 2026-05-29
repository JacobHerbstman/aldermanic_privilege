BANDWIDTH_FT ?= 500
WINDOW ?= pre_2023
CLUSTER_LEVEL ?= ward_pair
UNIT_DEF ?= unit_proxy
MIN_STRICTNESS_DIFF_PCTILE ?= 0
BINS_PER_SIDE ?= 8

all: \
	../output/char_fe_table_bw$(BANDWIDTH_FT)_clust_$(CLUSTER_LEVEL).tex \
	../output/amenity_char_fe_table_bw$(BANDWIDTH_FT)_$(WINDOW)_clust_$(CLUSTER_LEVEL).tex \
	../output/rent_amenity_attenuation_bw$(BANDWIDTH_FT)_$(WINDOW)_clust_$(CLUSTER_LEVEL).tex \
	../output/listing_units_$(UNIT_DEF)_bw$(BANDWIDTH_FT)_$(WINDOW)_all_pct$(MIN_STRICTNESS_DIFF_PCTILE)_clust_$(CLUSTER_LEVEL).pdf \
	../output/listing_units_ppml_fe_table_bw$(BANDWIDTH_FT)_$(WINDOW)_all_pct$(MIN_STRICTNESS_DIFF_PCTILE)_clust_$(CLUSTER_LEVEL).tex \
	../output/listing_units_$(UNIT_DEF)_bw$(BANDWIDTH_FT)_$(WINDOW)_multifamily_only_pct$(MIN_STRICTNESS_DIFF_PCTILE)_clust_$(CLUSTER_LEVEL).pdf \
	../output/listing_units_ppml_fe_table_bw$(BANDWIDTH_FT)_$(WINDOW)_multifamily_only_pct$(MIN_STRICTNESS_DIFF_PCTILE)_clust_$(CLUSTER_LEVEL).tex

.SECONDARY: \
	../temp/rent_with_ward_distances_amenities.parquet \
	../temp/listing_units_side_panel_bw$(BANDWIDTH_FT)_$(WINDOW)_all_pct$(MIN_STRICTNESS_DIFF_PCTILE)_$(UNIT_DEF)_all.parquet \
	../temp/listing_units_bin_cells_bw$(BANDWIDTH_FT)_$(WINDOW)_all_pct$(MIN_STRICTNESS_DIFF_PCTILE)_$(UNIT_DEF)_bins$(BINS_PER_SIDE)_all.parquet \
	../temp/listing_units_side_panel_bw$(BANDWIDTH_FT)_$(WINDOW)_multifamily_only_pct$(MIN_STRICTNESS_DIFF_PCTILE)_$(UNIT_DEF)_all.parquet \
	../temp/listing_units_bin_cells_bw$(BANDWIDTH_FT)_$(WINDOW)_multifamily_only_pct$(MIN_STRICTNESS_DIFF_PCTILE)_$(UNIT_DEF)_bins$(BINS_PER_SIDE)_all.parquet

../output/char_fe_table_bw$(BANDWIDTH_FT)_clust_$(CLUSTER_LEVEL).tex: characteristic_fe_table.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript characteristic_fe_table.R $(BANDWIDTH_FT) $(WINDOW) $(CLUSTER_LEVEL)

../output/amenity_char_fe_table_bw$(BANDWIDTH_FT)_$(WINDOW)_clust_$(CLUSTER_LEVEL).tex: amenity_characteristic_fe_table.R ../temp/rent_with_ward_distances_amenities.parquet | ../output
	Rscript amenity_characteristic_fe_table.R $(BANDWIDTH_FT) $(WINDOW) $(CLUSTER_LEVEL)

../output/rent_amenity_attenuation_bw$(BANDWIDTH_FT)_$(WINDOW)_clust_$(CLUSTER_LEVEL).tex: rent_amenity_attenuation.R ../temp/rent_with_ward_distances_amenities.parquet | ../output
	Rscript rent_amenity_attenuation.R $(BANDWIDTH_FT) $(WINDOW) $(CLUSTER_LEVEL)

../output/listing_units_$(UNIT_DEF)_bw$(BANDWIDTH_FT)_$(WINDOW)_%_pct$(MIN_STRICTNESS_DIFF_PCTILE)_clust_$(CLUSTER_LEVEL).pdf: listing_units_plot_cached.R ../temp/listing_units_side_panel_bw$(BANDWIDTH_FT)_$(WINDOW)_%_pct$(MIN_STRICTNESS_DIFF_PCTILE)_$(UNIT_DEF)_all.parquet ../temp/listing_units_bin_cells_bw$(BANDWIDTH_FT)_$(WINDOW)_%_pct$(MIN_STRICTNESS_DIFF_PCTILE)_$(UNIT_DEF)_bins$(BINS_PER_SIDE)_all.parquet | ../output
	Rscript listing_units_plot_cached.R $(BANDWIDTH_FT) $(WINDOW) $* $(UNIT_DEF) $(MIN_STRICTNESS_DIFF_PCTILE) $(BINS_PER_SIDE) $(CLUSTER_LEVEL)

../output/listing_units_ppml_fe_table_bw$(BANDWIDTH_FT)_$(WINDOW)_%_pct$(MIN_STRICTNESS_DIFF_PCTILE)_clust_$(CLUSTER_LEVEL).tex: listing_units_ppml_fe_table_cached.R ../temp/listing_units_side_panel_bw$(BANDWIDTH_FT)_$(WINDOW)_%_pct$(MIN_STRICTNESS_DIFF_PCTILE)_$(UNIT_DEF)_all.parquet | ../output
	Rscript listing_units_ppml_fe_table_cached.R $(BANDWIDTH_FT) $(WINDOW) $* $(UNIT_DEF) $(MIN_STRICTNESS_DIFF_PCTILE) $(BINS_PER_SIDE) $(CLUSTER_LEVEL)

../temp/rent_with_ward_distances_amenities.parquet: enrich_rent_with_amenity_distances.R ../input/rent_with_ward_distances.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../temp
	Rscript enrich_rent_with_amenity_distances.R

../temp/listing_units_side_panel_bw$(BANDWIDTH_FT)_$(WINDOW)_%_pct$(MIN_STRICTNESS_DIFF_PCTILE)_$(UNIT_DEF)_all.parquet: build_listing_units_cache.R ../input/rent_with_ward_distances.parquet | ../temp
	Rscript build_listing_units_cache.R $(BANDWIDTH_FT) $(WINDOW) $* $(UNIT_DEF) $(MIN_STRICTNESS_DIFF_PCTILE) $(BINS_PER_SIDE)

../temp/listing_units_bin_cells_bw$(BANDWIDTH_FT)_$(WINDOW)_%_pct$(MIN_STRICTNESS_DIFF_PCTILE)_$(UNIT_DEF)_bins$(BINS_PER_SIDE)_all.parquet: ../temp/listing_units_side_panel_bw$(BANDWIDTH_FT)_$(WINDOW)_%_pct$(MIN_STRICTNESS_DIFF_PCTILE)_$(UNIT_DEF)_all.parquet
	@test -s "$@"

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../prepare_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../prepare_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg: ../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../generic.make
