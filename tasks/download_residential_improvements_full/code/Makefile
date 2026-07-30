START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/residential_improvement_characteristics_full.csv

../output/residential_improvement_characteristics_full.csv: download_residential_improvements.sh Makefile | ../output
	bash $< $(START_YEAR) $(END_YEAR)

include ../../generic.make
