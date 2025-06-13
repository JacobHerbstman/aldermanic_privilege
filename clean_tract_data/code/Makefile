SHELL := bash

# pull in the generic rules (which also exports PROJECT_ROOT for R if you need it)
include ../../shell_functions.make

# default target
.DEFAULT_GOAL := ../output/census_tracts.shp

# produce the tract‐shapefile output
# ──────────────────────────────────────────────────────────────────────────────
../output/census_tracts.shp: census_tracts_tidy.R | ../output
	Rscript census_tracts_tidy.R

# ensure the output directory exists
../output:
	mkdir -p $@

include ../../generic.make


