SHELL := bash

# 1) Bring in generic rules (exports PROJECT_ROOT, makes ../input & ../output, etc)
include ../../shell_functions.make

# 2) Default target
.DEFAULT_GOAL := ../output/census_tract_panel_wards.shp

# 3) Phony step to populate the input folder via symlinks
.PHONY: setup-input
setup-input: ../input
	# Relative to *this* code/ folder:
	#   clean_tract_data/output/      ── is two levels up and then down
	ln -sf ../../clean_tract_data/output/census_tracts.* \
	       ../input/

# 4) Build the final panel shapefile
../output/census_tract_panel_wards.shp: \
        merge_wards_tracts.R \
        ../input/CHI_2005/CHI_2005.shp \
        ../input/CHI_2015/CHI_2015.shp \
        | ../output setup-input
	Rscript $<

include ../../generic.make
