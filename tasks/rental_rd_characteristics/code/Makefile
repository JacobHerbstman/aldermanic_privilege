SHELL := bash

BANDWIDTH_FT ?= 500

all: \
	../output/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet \
	../output/rental_rd_amenity_distance_diagnostics_bw$(BANDWIDTH_FT).csv \
	../output/rental_rd_cta_station_assignment_bw$(BANDWIDTH_FT).csv \
	../output/rental_rd_covariate_balance_bw$(BANDWIDTH_FT).csv \
	../output/rental_rd_external_amenity_balance_bw$(BANDWIDTH_FT).tex \
	../output/rental_rd_covariate_balance_bw$(BANDWIDTH_FT).pdf \
	../output/rental_rd_paired_covariate_balance_bw$(BANDWIDTH_FT).csv \
	../output/rental_rd_paired_external_amenity_balance_bw$(BANDWIDTH_FT).tex \
	../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).csv \
	../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).tex \
	../output/rental_rd_rent_attenuation_clean_location_bw$(BANDWIDTH_FT).tex \
	../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).pdf \
	../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).png

../output/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet: build_rental_rd_characteristics.R ../input/rent_with_ward_distances.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	$(R) $< $(BANDWIDTH_FT)

../output/rental_rd_amenity_distance_diagnostics_bw$(BANDWIDTH_FT).csv \
../output/rental_rd_cta_station_assignment_bw$(BANDWIDTH_FT).csv \
../output/rental_rd_covariate_balance_bw$(BANDWIDTH_FT).csv \
../output/rental_rd_external_amenity_balance_bw$(BANDWIDTH_FT).tex \
../output/rental_rd_covariate_balance_bw$(BANDWIDTH_FT).pdf \
../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).csv \
../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).tex \
../output/rental_rd_rent_attenuation_clean_location_bw$(BANDWIDTH_FT).tex \
../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).pdf \
../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).png: ../output/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet
	@test -s "$@"

../output/rental_rd_paired_covariate_balance_bw$(BANDWIDTH_FT).csv: build_rental_rd_paired_balance.R ../output/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet | ../output
	$(R) $< $(BANDWIDTH_FT)

../output/rental_rd_paired_external_amenity_balance_bw$(BANDWIDTH_FT).tex: ../output/rental_rd_paired_covariate_balance_bw$(BANDWIDTH_FT).csv
	@test -s "$@"

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../prepare_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../prepare_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cta_stops.gpkg: ../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.dbf | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.prj | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shx | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.cpg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../shell_functions.make
include ../../generic.make
