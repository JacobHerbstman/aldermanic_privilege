include ../../shared/code/shell_functions.make

all: ../output/chicago_gis_layers.csv ../output/zoning_districts_current.geojson ../output/zoning_districts_2012.zip \
	../output/street_centerlines_2013.zip

../%/chicago_gis_layers.csv ../%/zoning_districts_current.geojson ../%/zoning_districts_2012.zip \
	../%/street_centerlines_2013.zip: download_chicago_gis_layers.R | ../output ../report ../temp
	$(R) $<

include ../../shared/code/generic.make
