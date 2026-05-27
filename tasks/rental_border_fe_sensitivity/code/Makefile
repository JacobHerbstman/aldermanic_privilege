SHELL := bash

BANDWIDTH_FT ?= 500
WINDOW ?= pre_2023
SAMPLE_FILTER ?= all
USE_CONTROLS ?= TRUE
N_PERMS ?= 500
SEED ?= 42
MIN_DATE ?= 2015-05-18

all: ../output/permutation_test_2014_2022_all_bw500_ctrl_clust_ward_pair.pdf

../output/permutation_test_2014_2022_all_bw500_ctrl_clust_ward_pair.pdf: run_permutation_test.R ../input/rent_with_ward_distances.parquet | ../output ../temp
	Rscript run_permutation_test.R $(BANDWIDTH_FT) $(WINDOW) $(SAMPLE_FILTER) $(USE_CONTROLS) $(N_PERMS) $(SEED) $(MIN_DATE)

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../generic.make
