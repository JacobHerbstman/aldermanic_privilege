include ../../shared/code/shell_functions.make

all: ../output/residential_assessor_history.parquet

../output/residential_assessor_history.parquet: prepare_assessor_history.R ../../setup_environment/code/packages.R ../input/residential_improvement_characteristics_full.csv ../adjudication/parcel_reference_corrections.csv Makefile ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../input/residential_improvement_characteristics_full.csv: ../../../data_raw/construction_review/residential_improvement_characteristics_full.csv | ../input
	ln -sf $< $@

link-inputs: ../input/residential_improvement_characteristics_full.csv

include ../../shared/code/generic.make
