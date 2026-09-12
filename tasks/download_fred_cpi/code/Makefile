include ../../shared/code/shell_functions.make

SERIES_ID := CUURA207SA0

all: ../output/fred_cpi_cuura207sa0.csv

download-current: ../output/fred_cpi_cuura207sa0_current.csv

../output/fred_cpi_cuura207sa0.csv: ../../../data_raw/replication_sources/fred_cpi_cuura207sa0_20260731.csv source_snapshot.sha256 Makefile | ../output
	shasum -a 256 -c source_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

../output/fred_cpi_cuura207sa0_current.csv: download_fred_cpi.R Makefile ../../setup_environment/code/packages.R | ../output
	$(R) $< $(SERIES_ID)

.PHONY: download-current

include ../../shared/code/generic.make
