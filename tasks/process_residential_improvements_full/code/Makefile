SHELL := bash
include ../../shell_functions.make

../output/residential_improvements_panel.parquet: process_improvements.R ../input/residential_improvement_characteristics_full.csv | ../output
	@echo "→ [process_residential_improvements_full] Processing residential improvements to panel..."
	$(R) $<
	@echo "→ [process_residential_improvements_full] Done"

include ../../generic.make
