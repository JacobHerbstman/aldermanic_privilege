all: ../output/residential_improvements_panel.parquet

../output/residential_improvements_panel.parquet: process_improvements.R ../input/residential_improvement_characteristics_full.csv | ../output
	Rscript $<

../input/residential_improvement_characteristics_full.csv: ../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/residential_improvement_characteristics_full.csv

include ../../generic.make
