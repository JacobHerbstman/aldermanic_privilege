BANDWIDTH_M ?= 152.4
BANDWIDTH_LABEL ?= 500ft
PERMUTATIONS ?= 9999
SEED ?= 42
CORES ?= 8

all: ../output/permit_itt_score_permutation_$(BANDWIDTH_LABEL)_B$(PERMUTATIONS).pdf

../output/permit_itt_score_permutation_$(BANDWIDTH_LABEL)_B$(PERMUTATIONS).pdf: run_score_permutation.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< $(BANDWIDTH_M) $(BANDWIDTH_LABEL) $(PERMUTATIONS) $(SEED) $(CORES)

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet

include ../../../generic.make
