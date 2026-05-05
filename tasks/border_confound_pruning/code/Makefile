SHELL := bash
include ../../shell_functions.make

all: \
	../output/confounded_pair_era_flags.csv \
	../output/keep_pair_era.csv \
	../output/drop_pair_era.csv \
	../output/confound_pruning_summary.md

../output/confounded_pair_era_flags.csv: build_confound_flags.R ../input/segment_classification.csv | ../output
	Rscript build_confound_flags.R
	test -f ../output/keep_pair_era.csv
	test -f ../output/drop_pair_era.csv
	test -f ../output/confound_pruning_summary.md

../output/keep_pair_era.csv: ../output/confounded_pair_era_flags.csv
	test -f "$@"

../output/drop_pair_era.csv: ../output/confounded_pair_era_flags.csv
	test -f "$@"

../output/confound_pruning_summary.md: ../output/confounded_pair_era_flags.csv
	test -f "$@"

../input/segment_classification.csv: ../../border_segment_creation/output/segment_classification.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/segment_classification.csv

include ../../generic.make
