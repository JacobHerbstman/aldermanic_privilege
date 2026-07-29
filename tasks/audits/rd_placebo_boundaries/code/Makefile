BANDWIDTH_FT ?= 500
PLACEBO_CUTOFFS_FT ?= -500,0,500

all: \
	../output/rd_placebo_boundary_estimates.csv \
	../output/rd_placebo_boundary_support.csv \
	../output/rent_placebo_boundaries.pdf \
	../output/rent_placebo_boundaries.png \
	../output/sales_placebo_boundaries.pdf \
	../output/sales_placebo_boundaries.png \
	../output/rd_placebo_boundary_score_gap_split_estimates.csv \
	../output/rd_placebo_boundary_score_gap_split_support.csv \
	../output/rent_placebo_boundaries_score_gap_split.pdf \
	../output/rent_placebo_boundaries_score_gap_split.png \
	../output/sales_placebo_boundaries_score_gap_split.pdf \
	../output/sales_placebo_boundaries_score_gap_split.png

../output/rd_placebo_boundary_estimates.csv: build_rd_placebo_boundaries.R ../input/rent_with_ward_distances.parquet ../input/sales_with_hedonics_amenities.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript $< $(BANDWIDTH_FT) $(PLACEBO_CUTOFFS_FT)

../output/rd_placebo_boundary_support.csv \
../output/rent_placebo_boundaries.pdf \
../output/rent_placebo_boundaries.png \
../output/sales_placebo_boundaries.pdf \
../output/sales_placebo_boundaries.png \
../output/rd_placebo_boundary_score_gap_split_estimates.csv \
../output/rd_placebo_boundary_score_gap_split_support.csv \
../output/rent_placebo_boundaries_score_gap_split.pdf \
../output/rent_placebo_boundaries_score_gap_split.png \
../output/sales_placebo_boundaries_score_gap_split.pdf \
../output/sales_placebo_boundaries_score_gap_split.png: ../output/rd_placebo_boundary_estimates.csv
	@test -s "$@"

../input/rent_with_ward_distances.parquet: ../../../merge_rent_scores/output/rent_with_ward_distances_full_through2022.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../../prepare_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../../prepare_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cta_stops.gpkg: ../../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.dbf: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.dbf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.prj: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.prj | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shx: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shx | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.cpg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet ../input/sales_with_hedonics_amenities.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../../generic.make
