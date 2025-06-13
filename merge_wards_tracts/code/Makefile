SHELL := bash

# 1) Bring in generic rules (exports PROJECT_ROOT, makes ../input & ../output, etc)
include ../../generic.make

# 2) Default target
all: ../output/census_tract_panel_wards.shp

# 3) Phony step to populate the input folder via symlinks
.PHONY: setup-input
setup-input: ../input
	# Link the cleaned census‐tract shapefile (and its .dbf/.shx/.prj)
	ln -snf $(PROJECT_ROOT)/clean_tract_data/output/census_tracts.* \
	        ../input/

# 4) Build the final panel shapefile
#    Depends on your R script and all of the .shp inputs
../output/census_tract_panel_wards.shp: merge_wards_tracts.R \
	../input/CHI_2005/CHI_2005.shp \
	../input/CHI_2015/CHI_2015.shp \
	../input/census_tracts.shp | ../output setup-input
	Rscript merge_wards_tracts.R

