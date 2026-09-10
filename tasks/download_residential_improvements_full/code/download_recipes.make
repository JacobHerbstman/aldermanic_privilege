include ../../shared/code/shell_functions.make

START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/residential_improvement_characteristics_full_current.csv

../output/residential_improvement_characteristics_full_current.csv: download_residential_improvements.sh download_recipes.make | ../output
	bash $< $(START_YEAR) $(END_YEAR)

include ../../shared/code/generic.make
