all: ../output/cta_stops.gpkg ../output/major_streets.gpkg ../output/parks.gpkg ../output/schools_2015.gpkg

../output/cta_stops.gpkg: prepare_amenities_data.R ../input/major_streets.geojson ../input/cta_stations.geojson ../input/cpd_park_boundaries.geojson ../input/cps_school_locations_sy1415.geojson | ../output
	Rscript $<

../output/major_streets.gpkg ../output/parks.gpkg ../output/schools_2015.gpkg: ../output/cta_stops.gpkg
	@test -s "$@"

../input/major_streets.geojson: ../../download_chicago_spatial_data/output/major_streets.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cta_stations.geojson: ../../download_chicago_spatial_data/output/cta_stations.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cpd_park_boundaries.geojson: ../../download_park_boundaries_data/output/cpd_park_boundaries.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cps_school_locations_sy1415.geojson: ../../download_chicago_spatial_data/output/cps_school_locations_sy1415.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/major_streets.geojson ../input/cta_stations.geojson ../input/cpd_park_boundaries.geojson ../input/cps_school_locations_sy1415.geojson

include ../../generic.make
