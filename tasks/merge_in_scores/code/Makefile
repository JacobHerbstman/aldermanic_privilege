SHELL := bash
include ../../shell_functions.make

SCORE_COLUMN ?= strictness_index
SCORE_FILE_UPSTREAM ?= ../../create_rezoning_strictness_scores/output/alderman_rezoning_strictness_scores.csv
SCORE_FILE := ../input/aldermen_strictness_scores.csv

all: ../output/parcels_with_ward_distances.csv

../output/parcels_with_ward_distances.csv: merge_scores.R ../input/parcels_pre_scores.csv $(SCORE_FILE) | ../output
	$(R) merge_scores.R --score_file=$(SCORE_FILE) --score_column=$(SCORE_COLUMN)

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf $< $@

../input/aldermen_strictness_scores.csv: $(SCORE_FILE_UPSTREAM) | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_pre_scores.csv $(SCORE_FILE)
