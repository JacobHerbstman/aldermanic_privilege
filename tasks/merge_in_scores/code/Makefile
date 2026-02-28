SHELL := bash
include ../../shell_functions.make

# -----------------------------------------------------------------------------
# Parameters - change SCORE_SPEC to quickly swap score files
# -----------------------------------------------------------------------------

# Default score specification
SCORE_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH

# Column to use from the score file
SCORE_COLUMN ?= uncertainty_index

# Upstream score file (override to use strictness scores)
SCORE_FILE_UPSTREAM ?= ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(SCORE_SPEC).csv

SCORE_FILE := ../input/aldermen_uncertainty_scores.csv
SEGMENT_LOOKUP := ../input/parcel_segment_ids.csv

# -----------------------------------------------------------------------------
# Main Targets
# -----------------------------------------------------------------------------

all: ../output/parcels_with_ward_distances.csv

../output/parcels_with_ward_distances.csv: merge_scores.R ../input/parcels_pre_scores.csv $(SCORE_FILE) $(SEGMENT_LOOKUP) | ../output
	# arg order: score_file score_column
	$(R) merge_scores.R $(SCORE_FILE) $(SCORE_COLUMN)

# -----------------------------------------------------------------------------
# Input Symlinks
# -----------------------------------------------------------------------------

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf $< $@

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	ln -sf $< $@

# Note: Score file symlink - change the source to test different score specifications
../input/aldermen_uncertainty_scores.csv: $(SCORE_FILE_UPSTREAM) | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_pre_scores.csv $(SCORE_FILE) $(SEGMENT_LOOKUP)

# -----------------------------------------------------------------------------
# Helper targets for common score specifications
# -----------------------------------------------------------------------------

.PHONY: with-ca with-no-ca with-strictness

# CA FE + 2stage (default recommended spec)
with-ca:
	$(MAKE) SCORE_SPEC=ptfeFALSE_rtfeTRUE_porchTRUE_cafeTRUE_2stage

# No CA FE
with-no-ca:
	$(MAKE) SCORE_SPEC=ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE_2stage

# Legacy strictness score (month FE)
with-strictness:
	$(MAKE) SCORE_FILE_UPSTREAM=../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv
