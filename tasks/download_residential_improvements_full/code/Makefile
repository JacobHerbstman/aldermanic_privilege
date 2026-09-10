include ../../shared/code/shell_functions.make

all: ../output/residential_improvement_characteristics_full.csv ../report/residential_improvement_characteristics_full.csv.log

../output/residential_improvement_characteristics_full.csv: ../../../data_raw/replication_sources/residential_improvements_2006_2022_20260731.csv source_snapshot.sha256 Makefile | ../output
	shasum -a 256 -c source_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

../report/residential_improvement_characteristics_full.csv.log: ../../shared/code/report.py ../output/residential_improvement_characteristics_full.csv | ../report
	$(PYTHON) $< ../output/residential_improvement_characteristics_full.csv $@ row_id

include ../../shared/code/generic.make
