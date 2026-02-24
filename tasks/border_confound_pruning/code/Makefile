SHELL := bash
include ../../shell_functions.make

OUTPUTS := \
	../output/confounded_pair_era_flags.csv \
	../output/keep_pair_era.csv \
	../output/drop_pair_era.csv \
	../output/confound_pruning_summary.md

INPUTS := \
	../input/segment_classification.csv \
	../input/border_verification_invalid_pairs_sales.csv \
	../input/border_verification_invalid_pairs_rent.csv

all: $(OUTPUTS)

$(OUTPUTS): build_confound_flags.R $(INPUTS) | ../output
	Rscript build_confound_flags.R

../input/segment_classification.csv: ../../border_segment_creation/output/segment_classification.csv | ../input
	ln -sf "$<" "$@"

../input/border_verification_invalid_pairs_sales.csv: ../../border_verification/output/border_verification_invalid_pairs_sales.csv | ../input
	ln -sf "$<" "$@"

../input/border_verification_invalid_pairs_rent.csv: ../../border_verification/output/border_verification_invalid_pairs_rent.csv | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUTS)

include ../../generic.make
