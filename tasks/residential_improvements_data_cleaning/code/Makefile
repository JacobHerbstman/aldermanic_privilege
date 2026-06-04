all: ../output/residential_cross_section.csv

../output/residential_cross_section.csv: residential_improvements_cleaning.R ../input/residential_improvement_characteristics_full.csv | ../output
	Rscript $<

../input/residential_improvement_characteristics_full.csv: ../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/residential_improvement_characteristics_full.csv

include ../../generic.make
