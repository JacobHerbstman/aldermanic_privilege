SHELL := bash
include ../../shell_functions.make

all: link-inputs
	mkdir -p ../output
	$(R) diagnose_fe_rd_leverage.R

link-inputs:
	mkdir -p ../input
	ln -sf ../../merge_in_scores/output/parcels_with_ward_distances.csv ../input/parcels_with_ward_distances.csv
