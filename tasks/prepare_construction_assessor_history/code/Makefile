include ../../shared/code/shell_functions.make

all: ../output/residential_assessor_history.parquet ../report/residential_assessor_history.parquet.log

../output/residential_assessor_history.parquet: prepare_assessor_history.R ../../setup_environment/code/packages.R ../input/residential_improvement_characteristics_full.csv ../adjudication/parcel_reference_corrections.csv Makefile | ../output
	$(R) $<

../report/residential_assessor_history.parquet.log: ../../shared/code/report.py ../output/residential_assessor_history.parquet | ../report
	$(PYTHON) $< ../output/residential_assessor_history.parquet $@ row_id

../input/residential_improvement_characteristics_full.csv: ../../../data_raw/construction_review/residential_improvement_characteristics_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/residential_improvement_characteristics_full.csv

include ../../shared/code/generic.make
