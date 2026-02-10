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
	Rscript $< --input=$(INPUT_FILE) --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--use_controls=TRUE --n_perms=500 --seed=42 \
		--output_csv=$(PERM_CSV) --output_pdf=$@

$(INPUT_FILE): ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT_FILE)

include ../../generic.make
