SCORE_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022
SCORE_COLUMN ?= uncertainty_index
MAX_CONSTRUCTION_YEAR ?= 2026
ZONING_HISTORY_MAX_DISTANCE_FT ?= 1500

all: ../output/parcels_with_ward_distances.csv

../output/parcels_with_ward_distances.csv: merge_scores.R ../input/parcels_pre_scores.csv ../input/aldermen_uncertainty_scores.csv ../input/parcel_segment_ids.csv ../input/density_construction_zoning.csv | ../output
	Rscript $< $(SCORE_COLUMN) $(MAX_CONSTRUCTION_YEAR) $(ZONING_HISTORY_MAX_DISTANCE_FT)

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(SCORE_SPEC).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_construction_zoning.csv: ../../density_construction_zoning/output/density_construction_zoning.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_pre_scores.csv ../input/aldermen_uncertainty_scores.csv ../input/parcel_segment_ids.csv ../input/density_construction_zoning.csv

include ../../generic.make
