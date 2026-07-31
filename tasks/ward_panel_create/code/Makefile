START_YEAR ?= 2003
END_YEAR ?= 2022

all: ../output/ward_panel.gpkg

../output/ward_panel.gpkg: make_ward_panel.R ../input/Wards_2014.geojson ../input/Wards_2015.geojson Makefile | ../output
	$(R) $< $(START_YEAR) $(END_YEAR)

../input/Wards_2014.geojson: ../../../data_raw/Wards_2014.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/Wards_2015.geojson: ../../../data_raw/Wards_2015.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/Wards_2014.geojson ../input/Wards_2015.geojson

include ../../shell_functions.make
include ../../generic.make
