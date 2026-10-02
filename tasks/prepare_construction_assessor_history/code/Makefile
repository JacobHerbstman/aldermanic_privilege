include ../../shared/code/shell_functions.make

all: ../output/residential_assessor_history.parquet

../output/residential_assessor_history.parquet: prepare_assessor_history.R ../input/residential_improvement_characteristics_full.csv ../adjudication/parcel_reference_corrections.csv Makefile | ../output ../report
	$(R) $<

../input/residential_improvement_characteristics_full.csv: ../../../data_raw/construction_review/residential_improvement_characteristics_full.csv | ../input
	ln -sf $< $@

link-inputs: ../input/residential_improvement_characteristics_full.csv

include ../../shared/code/generic.make
