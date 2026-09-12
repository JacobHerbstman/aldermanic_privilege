include ../../shared/code/shell_functions.make

START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/residential_improvements_panel.parquet

../output/residential_improvements_panel.parquet: process_improvements.R ../input/residential_improvement_characteristics_full.csv Makefile ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $< $(START_YEAR) $(END_YEAR)

../input/residential_improvement_characteristics_full.csv: ../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv | ../input
	ln -sf $< $@

link-inputs: ../input/residential_improvement_characteristics_full.csv

include ../../shared/code/generic.make
