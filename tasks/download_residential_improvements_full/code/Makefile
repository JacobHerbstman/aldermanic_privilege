include ../../shared/code/shell_functions.make

START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/residential_improvement_characteristics_full.csv

download-current: ../output/residential_improvement_characteristics_full_current.csv

../output/residential_improvement_characteristics_full.csv: ../../../data_raw/replication_sources/residential_improvements_2006_2022_20260731.csv source_snapshot.sha256 Makefile | ../output
	shasum -a 256 -c source_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

../output/residential_improvement_characteristics_full_current.csv: download_residential_improvements.sh Makefile | ../output
	bash $< $(START_YEAR) $(END_YEAR)

.PHONY: download-current

include ../../shared/code/generic.make
