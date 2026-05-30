BALANCE_BW_M ?= 152.4
BALANCE_BW_LABEL ?= 500ft

all: \
	../output/density_paired_balance_$(BALANCE_BW_LABEL)_all.tex \
	../output/density_paired_balance_$(BALANCE_BW_LABEL)_multifamily.tex

../output/density_paired_balance_$(BALANCE_BW_LABEL)_all.tex: build_density_amenity_balance.R ../../_lib/amenity_distance_helpers.R ../../_lib/border_pair_helpers.R ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript $< $(BALANCE_BW_M) all $(BALANCE_BW_LABEL)

../output/density_paired_balance_$(BALANCE_BW_LABEL)_multifamily.tex: build_density_amenity_balance.R ../../_lib/amenity_distance_helpers.R ../../_lib/border_pair_helpers.R ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript $< $(BALANCE_BW_M) multifamily $(BALANCE_BW_LABEL)

../input/parcels_with_geometry.gpkg: ../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../prepare_amenities_data/output/schools_2015.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parks.gpkg: ../../prepare_amenities_data/output/parks.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../prepare_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/parcels_with_geometry.gpkg \
	../input/parcels_with_ward_distances.csv \
	../input/schools_2015.gpkg \
	../input/parks.gpkg \
	../input/major_streets.gpkg \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg

include ../../generic.make
