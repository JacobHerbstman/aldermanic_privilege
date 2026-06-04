all: ../output/residential_improvement_characteristics_full.csv

../output/residential_improvement_characteristics_full.csv: download_residential_improvements.sh | ../output
	bash $<

include ../../generic.make
