include ../../shared/code/shell_functions.make

all: ../output/chicago_building_footprints_2015.zip

../output/chicago_building_footprints_2015.zip: Makefile source_snapshot.sha256 | ../output ../temp
	curl --fail --location 'https://data.cityofchicago.org/api/views/syp8-uezg/files/Wir2BTHPb7-BTOWMkr8XcKCfKCt8U6y8wK20cV4Tjhw?download=true&filename=buildings.zip' --output ../temp/chicago_building_footprints_2015.zip
	shasum -a 256 -c source_snapshot.sha256
	unzip -tq ../temp/chicago_building_footprints_2015.zip
	mv ../temp/chicago_building_footprints_2015.zip $@

include ../../shared/code/generic.make
