all: ../output/census_blocks_2020.csv

../output/census_blocks_2020.csv: clean_blocks.R ../input/tl_2025_17_tabblock20.shp ../input/tl_2025_17_tabblock20.dbf ../input/tl_2025_17_tabblock20.prj ../input/tl_2025_17_tabblock20.shx ../input/city_boundary.geojson | ../output
	Rscript $<

../input/tl_2025_17_tabblock20.shp ../input/tl_2025_17_tabblock20.dbf ../input/tl_2025_17_tabblock20.prj ../input/tl_2025_17_tabblock20.shx: ../input/tl_2025_17_tabblock20.%: ../../../data_raw/tl_2025_17_tabblock20.% | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/city_boundary.geojson: ../../../data_raw/Boundaries_-_City_20250920.geojson | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/tl_2025_17_tabblock20.shp ../input/tl_2025_17_tabblock20.dbf ../input/tl_2025_17_tabblock20.prj ../input/tl_2025_17_tabblock20.shx ../input/city_boundary.geojson

include ../../generic.make
