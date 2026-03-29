SHELL := bash
include ../../shell_functions.make

all: ../output/census_blocks_2020.csv ../output/census_blocks_2020_boundary_diagnostics.csv

../output/census_blocks_2020.csv: clean_blocks.R ../input/tl_2025_17_tabblock20.shp ../input/census_blocks_2010.csv ../input/city_boundary.geojson | ../output
	$(R) clean_blocks.R

../output/census_blocks_2020_boundary_diagnostics.csv: ../output/census_blocks_2020.csv
	test -f $@

../input/tl_2025_17_tabblock20.shp: ../../../data_raw/tl_2025_17_tabblock20.shp | ../input
	ln -sf ../../../data_raw/tl_2025_17_tabblock20.* $(dir $@)

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

../input/city_boundary.geojson: ../../../data_raw/Boundaries_-_City_20250920.geojson | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/tl_2025_17_tabblock20.shp ../input/census_blocks_2010.csv ../input/city_boundary.geojson
