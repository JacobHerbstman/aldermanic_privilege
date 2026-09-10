include ../../shared/code/shell_functions.make

all: ../output/fred_cpi_cuura207sa0.csv ../report/fred_cpi_cuura207sa0.csv.log

../output/fred_cpi_cuura207sa0.csv: ../../../data_raw/replication_sources/fred_cpi_cuura207sa0_20260731.csv source_snapshot.sha256 Makefile | ../output
	shasum -a 256 -c source_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

../report/fred_cpi_cuura207sa0.csv.log: ../../shared/code/report.py ../output/fred_cpi_cuura207sa0.csv | ../report
	$(PYTHON) $< ../output/fred_cpi_cuura207sa0.csv $@ observation_date

include ../../shared/code/generic.make
