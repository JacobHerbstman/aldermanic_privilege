TARGET_SEGMENT_LENGTH_FT ?= 1320

all: \
	../output/confounded_pair_era_flags.csv \
	../output/confounded_segment_flags.csv

../output/confounded_pair_era_flags.csv: build_confound_flags.R ../input/segment_classification.csv | ../output
	Rscript $< $(TARGET_SEGMENT_LENGTH_FT)
	@test -s ../output/confounded_segment_flags.csv

../output/confounded_segment_flags.csv: ../output/confounded_pair_era_flags.csv
	@test -s "$@"

../input/segment_classification.csv: ../../border_segment_creation/output/segment_classification.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/segment_classification.csv

include ../../generic.make
