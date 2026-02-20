SHELL := bash
include ../../shell_functions.make

INPUT_FILE := ../input/rent_with_ward_distances.parquet

# Active specification used by slides_uncertainty appendix.
PERM_PDF := ../output/permutation_test_2014_2022_all_bw500_ctrl.pdf
PERM_CSV := ../output/permutation_test_2014_2022_all_bw500_ctrl.csv

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(PERM_PDF)

$(PERM_PDF): run_permutation_test.R $(INPUT_FILE) | ../output
	# arg order: input bw_ft window sample_filter use_controls n_perms seed min_date output_csv output_pdf
	Rscript $< $(INPUT_FILE) 500 pre_2023 all TRUE 500 42 2015-05-18 $(PERM_CSV) $@

$(INPUT_FILE): ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT_FILE)

include ../../generic.make
