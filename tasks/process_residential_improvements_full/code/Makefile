START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/residential_improvements_panel.parquet

../output/residential_improvements_panel.parquet: process_improvements.R ../input/residential_improvement_characteristics_full.csv Makefile | ../output
	$(R) $< $(START_YEAR) $(END_YEAR)

../input/residential_improvement_characteristics_full.csv: ../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/residential_improvement_characteristics_full.csv

include ../../shell_functions.make
include ../../generic.make
