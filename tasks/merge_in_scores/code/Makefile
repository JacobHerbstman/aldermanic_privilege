SHELL := bash
include ../../shell_functions.make

SCORE_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2023
SCORE_COLUMN ?= uncertainty_index

all: ../output/parcels_with_ward_distances.csv

../output/parcels_with_ward_distances.csv: merge_scores.R ../input/parcels_pre_scores.csv ../input/aldermen_uncertainty_scores.csv ../input/parcel_segment_ids.csv | ../output
	Rscript merge_scores.R ../input/aldermen_uncertainty_scores.csv $(SCORE_COLUMN)

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(SCORE_SPEC).csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_pre_scores.csv ../input/aldermen_uncertainty_scores.csv ../input/parcel_segment_ids.csv

include ../../generic.make
