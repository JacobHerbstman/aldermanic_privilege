SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# The main target for this task is the final CSV output
all: ../output/alderman_sentiment_scores.csv

# Build rule: The output depends on the script and all its inputs.
../output/alderman_sentiment_scores.csv: sentiment_alderman_statements.R ../input/alderman_prerogative_statements.csv aldermen_strictness_scores.csv  | ../output
	$(R) $<

## symlink
../input/alderman_prerogative_statements.csv: ../../scrape_alderman_statements/output/alderman_prerogative_statements.csv | ../input
	ln -sf "$<" "$@"

../input/aldermen_strictness_scores.csv: ../../rank_aldermen/output/aldermen_strictness_scores.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/alderman_prerogative_statements.csv ../input/aldermen_strictness_scores.csv

# Include generic rules for creating directories and cleaning
include ../../generic.make

