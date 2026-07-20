all: ../output/permits_for_uncertainty_index.csv

../output/permits_for_uncertainty_index.csv: prepare_uncertainty_data.R \
	../../_lib/canonical_geometry_helpers.R \
	../input/ward_panel.gpkg \
	../input/building_permits_clean.gpkg \
	../input/chicago_alderman_terms.csv \
	../input/ward_controls.csv \
	../input/community_areas.geojson \
	../input/cta_stops.gpkg \
	../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf \
	../input/gis_osm_water_a_free_1.prj \
	../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript $<

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_controls.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/community_areas.geojson: ../../download_chicago_spatial_data/output/community_areas.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cta_stops.gpkg: ../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/ward_panel.gpkg ../input/building_permits_clean.gpkg ../input/chicago_alderman_terms.csv ../input/ward_controls.csv ../input/community_areas.geojson ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../generic.make
