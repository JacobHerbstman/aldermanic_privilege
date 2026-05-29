SCORE_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022
SCORE_COLUMN ?= uncertainty_index
MAX_CONSTRUCTION_YEAR ?= 2026

all: ../output/parcels_with_ward_distances.csv

../output/parcels_with_ward_distances.csv: merge_scores.R ../input/parcels_pre_scores.csv ../input/aldermen_uncertainty_scores.csv ../input/parcel_segment_ids.csv | ../output
	MAX_CONSTRUCTION_YEAR=$(MAX_CONSTRUCTION_YEAR) Rscript merge_scores.R $(SCORE_COLUMN)

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(SCORE_SPEC).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_pre_scores.csv ../input/aldermen_uncertainty_scores.csv ../input/parcel_segment_ids.csv

include ../../generic.make
