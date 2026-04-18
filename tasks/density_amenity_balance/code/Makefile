SHELL := bash
include ../../shell_functions.make

all: ../output/density_amenity_balance_bw500_multifamily.tex

../output/density_amenity_balance_bw500_multifamily.csv ../output/density_amenity_balance_bw500_multifamily.tex: build_density_amenity_balance.R ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp | ../output
	Rscript build_density_amenity_balance.R ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../output/density_amenity_balance_bw500_multifamily.csv ../output/density_amenity_balance_bw500_multifamily.tex

../input/parcels_with_geometry.gpkg: ../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../download_amenities_data/output/schools_2015.gpkg | ../input
	ln -sf "$<" "$@"

../input/parks.gpkg: ../../download_amenities_data/output/parks.gpkg | ../input
	ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../download_amenities_data/output/major_streets.gpkg | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	ln -sf ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.* $(dir $@)

link-inputs: ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp

include ../../generic.make
