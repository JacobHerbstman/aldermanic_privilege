all: \
	../output/major_streets.geojson \
	../output/cta_stations.geojson \
	../output/cps_school_locations_sy1415.geojson \
	../output/community_areas.geojson \
	../output/city_boundary.geojson \
	../output/census_blocks_2010.csv

../output/major_streets.geojson: download_major_streets.R | ../output ../temp
	Rscript $<

../output/cta_stations.geojson: download_chicago_spatial_data.sh | ../output ../temp
	bash $< cta_stations

../output/cps_school_locations_sy1415.geojson: download_chicago_spatial_data.sh | ../output ../temp
	bash $< cps_school_locations_sy1415

../output/community_areas.geojson: download_chicago_spatial_data.sh | ../output ../temp
	bash $< community_areas

../output/city_boundary.geojson: download_chicago_spatial_data.sh | ../output ../temp
	bash $< city_boundary

../output/census_blocks_2010.csv: download_chicago_spatial_data.sh | ../output ../temp
	bash $< census_blocks_2010

include ../../generic.make
