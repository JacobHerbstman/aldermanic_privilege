SHELL := bash
include ../../shell_functions.make

all: ../output/residential_improvement_characteristics_full.csv

../output/residential_improvement_characteristics_full.csv: download_residential_improvements.sh | ../output
	@echo "→ [download_residential_improvements_full] Downloading full residential improvements from Socrata API..."
	bash $<
	@echo "→ [download_residential_improvements_full] Download complete"

live-download: ../output/residential_improvement_characteristics_full.csv

include ../../generic.make
