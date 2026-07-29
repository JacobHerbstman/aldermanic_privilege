BANDWIDTH_M ?= 152.4
BANDWIDTH_LABEL ?= 500ft
MATCH_CALIPERS_FT ?= 2640,5280
MATCH_LABEL ?= 2640-5280
BOUNDARY_DISTANCE_CALIPER_FT ?= 100
PERMUTATIONS ?= 999
SEED ?= 20260714

all: ../output/permit_itt_spatial_permutation_$(BANDWIDTH_LABEL)_match$(MATCH_LABEL)_dist$(BOUNDARY_DISTANCE_CALIPER_FT)_B$(PERMUTATIONS).pdf

../output/permit_itt_spatial_permutation_$(BANDWIDTH_LABEL)_match$(MATCH_LABEL)_dist$(BOUNDARY_DISTANCE_CALIPER_FT)_B$(PERMUTATIONS).pdf: run_spatial_permutation.R ../input/permit_block_year_panel_2015.parquet ../input/boundary_segments_1320ft.gpkg | ../output
	Rscript $< $(BANDWIDTH_M) $(BANDWIDTH_LABEL) $(MATCH_CALIPERS_FT) $(MATCH_LABEL) $(BOUNDARY_DISTANCE_CALIPER_FT) $(PERMUTATIONS) $(SEED)

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/boundary_segments_1320ft.gpkg

include ../../../generic.make
