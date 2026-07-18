BANDWIDTH_FT ?= 500
BINS_PER_SIDE ?= 10
START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/sales_rd_flat_bw$(BANDWIDTH_FT)_year_quarter_amenity_clust_segment.pdf

../output/sales_with_hedonics_amenities.parquet: enrich_sales_with_amenity_distances.R ../../_lib/amenity_distance_helpers.R ../input/sales_with_hedonics.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript $<

../output/sales_rd_flat_bw$(BANDWIDTH_FT)_year_quarter_amenity_clust_segment.pdf: sales_level_plot.R ../output/sales_with_hedonics_amenities.parquet | ../output
	Rscript $< $(BANDWIDTH_FT) $(BINS_PER_SIDE) $(START_YEAR) $(END_YEAR)

../output/sales_rd_flat_bw$(BANDWIDTH_FT)_year_quarter_amenity_clust_segment_panel.pdf: ../output/sales_rd_flat_bw$(BANDWIDTH_FT)_year_quarter_amenity_clust_segment.pdf
	@test -s "$@"

../input/sales_with_hedonics.parquet: ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../prepare_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../prepare_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cta_stops.gpkg: ../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/sales_with_hedonics.parquet \
	../input/schools_2015.gpkg \
	../input/parks.gpkg \
	../input/major_streets.gpkg \
	../input/cta_stops.gpkg \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg

include ../../generic.make
