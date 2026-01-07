SHELL := bash
include ../../shell_functions.make

../output/residential_improvement_characteristics_full.csv: download_residential_improvements.sh | ../output
	@echo "→ [download_residential_improvements_full] Downloading full residential improvements from Socrata API..."
	chmod +x $<
	bash $<
	@echo "→ [download_residential_improvements_full] Download complete"

include ../../generic.make
