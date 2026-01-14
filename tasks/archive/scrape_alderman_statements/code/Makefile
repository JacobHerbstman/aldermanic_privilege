SHELL := bash
include ../../shell_functions.make

# The main target for this task is the final CSV output
all: ../output/alderman_prerogative_statements.csv

# Build rule: The output depends on the script and all its inputs.
../output/alderman_prerogative_statements.csv: scrape_alderman_statements.R | ../output
	$(R) $<

# Include generic rules for creating directories and cleaning
include ../../generic.make

