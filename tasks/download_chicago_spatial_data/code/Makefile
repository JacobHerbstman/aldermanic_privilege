include ../../shared/code/shell_functions.make

all: ../output/census_blocks_2010.csv \
	../output/major_streets.geojson \
	../output/cta_stations.geojson \
	../output/cps_school_locations_sy1415.geojson \
	../output/community_areas.geojson

../output/major_streets.geojson: ../sources/major_streets.geojson.gz | ../output ../temp
	gzip -dc $< > ../temp/major_streets.geojson
	mv ../temp/major_streets.geojson $@

download-current: ../output/major_streets_current.geojson

../output/major_streets_current.geojson: download_major_streets.R | ../output ../temp
	$(R) $<

../output/cta_stations.geojson: download_chicago_spatial_data.sh | ../output ../temp
	bash $< cta_stations

../output/cps_school_locations_sy1415.geojson: download_chicago_spatial_data.sh | ../output ../temp
	bash $< cps_school_locations_sy1415

../output/community_areas.geojson: download_chicago_spatial_data.sh | ../output ../temp
	bash $< community_areas

../output/census_blocks_2010.csv: download_chicago_spatial_data.sh | ../output ../temp
	bash $< census_blocks_2010

include ../../shared/code/generic.make
