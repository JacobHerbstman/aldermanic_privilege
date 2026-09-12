include ../../shared/code/shell_functions.make

all: ../output/schools_2015.gpkg \
	../output/parks.gpkg \
	../output/major_streets.gpkg \
	../output/cta_stops.gpkg

../%/cta_stops.gpkg ../%/major_streets.gpkg ../%/parks.gpkg ../%/schools_2015.gpkg: prepare_amenities_data.R ../input/cta_opening_dates.csv ../input/cta_historical_stations.csv ../input/major_streets.geojson ../input/cta_stations.geojson ../input/cpd_park_boundaries.geojson ../input/cps_school_locations_sy1415.geojson ../../setup_environment/code/packages.R Makefile ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../input/major_streets.geojson: ../../download_chicago_spatial_data/output/major_streets.geojson | ../input
	ln -sf $< $@

../input/cta_stations.geojson: ../../download_chicago_spatial_data/output/cta_stations.geojson | ../input
	ln -sf $< $@

../input/cpd_park_boundaries.geojson: ../../download_park_boundaries_data/output/cpd_park_boundaries.geojson | ../input
	ln -sf $< $@

../input/cps_school_locations_sy1415.geojson: ../../download_chicago_spatial_data/output/cps_school_locations_sy1415.geojson | ../input
	ln -sf $< $@

../input/cta_opening_dates.csv: ../adjudication/cta_opening_dates.csv | ../input
	ln -sf $< $@

../input/cta_historical_stations.csv: ../adjudication/cta_historical_stations.csv | ../input
	ln -sf $< $@

link-inputs: ../input/cta_opening_dates.csv ../input/cta_historical_stations.csv ../input/major_streets.geojson ../input/cta_stations.geojson ../input/cpd_park_boundaries.geojson ../input/cps_school_locations_sy1415.geojson

include ../../shared/code/generic.make
