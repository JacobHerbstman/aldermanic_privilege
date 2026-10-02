include ../../shared/code/shell_functions.make

ACS_YEAR ?= 2014
GEOMETRY_YEAR ?= 2019

all: ../output/block_group_controls.csv

../output/block_group_controls.csv: create_block_group_controls.R ../output/acs_block_group_controls.csv ../output/block_group_geometry.gpkg Makefile | ../output ../report
	$(R) $<

../output/acs_block_group_controls.csv: ../sources/acs_block_group_controls.csv.gz | ../output ../temp
	gzip -dc $< > ../temp/acs_block_group_controls.csv
	mv ../temp/acs_block_group_controls.csv $@

../output/block_group_geometry.gpkg: ../sources/block_group_geometry.gpkg.gz | ../output ../temp
	gzip -dc $< > ../temp/block_group_geometry.gpkg
	mv ../temp/block_group_geometry.gpkg $@

download-current: ../output/acs_block_group_controls_current.csv ../output/block_group_geometry_current.gpkg

../%/acs_block_group_controls_current.csv ../%/block_group_geometry_current.gpkg: download_block_group_controls.R Makefile | ../output ../temp ../report
	$(R) $< $(ACS_YEAR) $(GEOMETRY_YEAR)

include ../../shared/code/generic.make
