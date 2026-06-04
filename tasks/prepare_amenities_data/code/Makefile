all: ../output/cta_stops.gpkg ../output/major_streets.gpkg ../output/parks.gpkg ../output/schools_2015.gpkg

../output/cta_stops.gpkg: prepare_amenities_data.R ../input/Major_Streets.shp ../input/Major_Streets.dbf ../input/Major_Streets.prj ../input/Major_Streets.shx ../input/cta_stations.geojson ../input/cpd_park_boundaries.geojson ../input/CPS_School_Locations_SY1415_20250925.geojson | ../output
	Rscript $<

../output/major_streets.gpkg ../output/parks.gpkg ../output/schools_2015.gpkg: ../output/cta_stops.gpkg
	@test -s "$@"

../input/Major_Streets.shp ../input/Major_Streets.dbf ../input/Major_Streets.prj ../input/Major_Streets.shx: ../input/Major_Streets.%: ../../../data_raw/Major_Streets.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cta_stations.geojson: ../../../data_raw/cta_stations.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cpd_park_boundaries.geojson: ../../download_park_boundaries_data/output/cpd_park_boundaries.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/CPS_School_Locations_SY1415_20250925.geojson: ../../../data_raw/CPS_School_Locations_SY1415_20250925.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/Major_Streets.shp ../input/Major_Streets.dbf ../input/Major_Streets.prj ../input/Major_Streets.shx ../input/cta_stations.geojson ../input/cpd_park_boundaries.geojson ../input/CPS_School_Locations_SY1415_20250925.geojson

include ../../generic.make
