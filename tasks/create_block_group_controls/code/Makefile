ACS_YEAR ?= 2014
GEOMETRY_YEAR ?= 2019

all: ../output/block_group_controls.csv

../output/block_group_controls.csv: create_block_group_controls.R Makefile | ../output
	Rscript $< $(ACS_YEAR) $(GEOMETRY_YEAR)

include ../../generic.make
