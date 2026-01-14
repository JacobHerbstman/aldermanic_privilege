SHELL := bash
include ../../shell_functions.make

all: ../output/census_blocks_2020.csv

../output/census_blocks_2020.csv: clean_blocks.R ../input/tl_2025_17_tabblock20.shp ../input/census_blocks_2010.csv | ../output
	$(R) clean_blocks.R

../input/tl_2025_17_tabblock20.shp: ../../../data_raw/tl_2025_17_tabblock20.shp | ../input
	ln -sf ../../../data_raw/tl_2025_17_tabblock20.* $(dir $@)

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/tl_2025_17_tabblock20.shp ../input/census_blocks_2010.csv
