SHELL := bash

TARGET_SEGMENT_LENGTH_FT ?= 1320

all: \
	../output/confounded_pair_era_flags.csv \
	../output/confounded_segment_flags.csv \
	../output/keep_pair_era.csv \
	../output/drop_pair_era.csv \
	../output/confound_pruning_segment_audit.csv \
	../output/confound_pruning_threshold_sensitivity.csv \
	../output/confound_pruning_summary.md

../output/confounded_pair_era_flags.csv: build_confound_flags.R ../input/segment_classification.csv | ../output
	TARGET_SEGMENT_LENGTH_FT=$(TARGET_SEGMENT_LENGTH_FT) Rscript build_confound_flags.R
	@test -s ../output/confounded_segment_flags.csv
	@test -s ../output/keep_pair_era.csv
	@test -s ../output/drop_pair_era.csv
	@test -s ../output/confound_pruning_segment_audit.csv
	@test -s ../output/confound_pruning_threshold_sensitivity.csv
	@test -s ../output/confound_pruning_summary.md

../output/confounded_segment_flags.csv: ../output/confounded_pair_era_flags.csv
	@test -s "$@"

../output/keep_pair_era.csv: ../output/confounded_pair_era_flags.csv
	@test -s "$@"

../output/drop_pair_era.csv: ../output/confounded_pair_era_flags.csv
	@test -s "$@"

../output/confound_pruning_segment_audit.csv: ../output/confounded_pair_era_flags.csv
	@test -s "$@"

../output/confound_pruning_threshold_sensitivity.csv: ../output/confounded_pair_era_flags.csv
	@test -s "$@"

../output/confound_pruning_summary.md: ../output/confounded_pair_era_flags.csv
	@test -s "$@"

../input/segment_classification.csv: ../../border_segment_creation/output/segment_classification.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/segment_classification.csv

include ../../shell_functions.make
include ../../generic.make
