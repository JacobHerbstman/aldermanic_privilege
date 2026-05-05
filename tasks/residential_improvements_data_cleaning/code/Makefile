SHELL := bash
include ../../shell_functions.make

all: ../output/residential_cross_section.csv

../output/residential_cross_section.csv: residential_improvements_cleaning.R ../input/residential_improvement_characteristics_full.csv | ../output
	$(R) residential_improvements_cleaning.R ../input/residential_improvement_characteristics_full.csv "$@"

../input/residential_improvement_characteristics_full.csv: ../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/residential_improvement_characteristics_full.csv
