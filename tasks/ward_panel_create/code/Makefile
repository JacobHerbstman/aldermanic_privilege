all: ../output/ward_panel.gpkg

../output/ward_panel.gpkg: make_ward_panel.R ../input/Chicago_Wards_1998.geojson ../input/Wards_2014.geojson ../input/Wards_2015.geojson ../input/Wards_2024.geojson | ../output
	Rscript $<

../input/Chicago_Wards_1998.geojson: ../../../data_raw/Chicago_Wards_1998.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/Wards_2014.geojson: ../../../data_raw/Wards_2014.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/Wards_2015.geojson: ../../../data_raw/Wards_2015.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/Wards_2024.geojson: ../../../data_raw/Wards_2024.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/Chicago_Wards_1998.geojson ../input/Wards_2014.geojson ../input/Wards_2015.geojson ../input/Wards_2024.geojson

include ../../generic.make
