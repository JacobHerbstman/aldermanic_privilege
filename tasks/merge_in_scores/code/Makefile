SHELL := bash
include ../../shell_functions.make

# -----------------------------------------------------------------------------
# Parameters - change SCORE_SPEC to quickly swap score files
# -----------------------------------------------------------------------------

# Default score specification
SCORE_SPEC ?= ptfeFALSE_rtfeTRUE_porchTRUE_cafeTRUE_2stage

# Column to use from the score file
SCORE_COLUMN ?= uncertainty_index

SCORE_FILE := ../input/aldermen_uncertainty_scores.csv

# -----------------------------------------------------------------------------
# Main Targets
# -----------------------------------------------------------------------------

all: ../output/parcels_with_ward_distances.csv

../output/parcels_with_ward_distances.csv: merge_scores.R ../input/parcels_pre_scores.csv $(SCORE_FILE) | ../output
	$(R) merge_scores.R --score_file=$(SCORE_FILE) --score_column=$(SCORE_COLUMN)

# -----------------------------------------------------------------------------
# Input Symlinks
# -----------------------------------------------------------------------------

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf $< $@

# Note: Score file symlink - change the source to test different score specifications
../input/aldermen_uncertainty_scores.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(SCORE_SPEC).csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_pre_scores.csv $(SCORE_FILE)

# -----------------------------------------------------------------------------
# Helper targets for common score specifications
# -----------------------------------------------------------------------------

.PHONY: with-ca with-no-ca

# CA FE + 2stage (default recommended spec)
with-ca:
	$(MAKE) SCORE_SPEC=ptfeFALSE_rtfeTRUE_porchTRUE_cafeTRUE_2stage

# No CA FE
with-no-ca:
	$(MAKE) SCORE_SPEC=ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE_2stage
