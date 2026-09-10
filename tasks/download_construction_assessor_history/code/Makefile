include ../../shared/code/shell_functions.make

START_YEAR ?= 1999
END_YEAR ?= 2026

all: ../output/residential_improvement_characteristics_full.csv ../output/commercial_valuation_data.csv

../output/residential_improvement_characteristics_full.csv: ../../download_residential_improvements_full/code/download_residential_improvements.sh Makefile | ../output
	bash $< $(START_YEAR) $(END_YEAR)

../output/commercial_valuation_data.csv: download_commercial_valuation_data.sh | ../output
	bash $<

all: ../report/construction_condominium_history.csv.log

../output/construction_condominium_history.csv: ../../../data_raw/construction_review/construction_condominium_history_20260909.csv condominium_snapshot.sha256 Makefile | ../output
	shasum -a 256 -c condominium_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

../report/construction_condominium_history.csv.log: ../../shared/code/report.py ../output/construction_condominium_history.csv | ../report
	$(PYTHON) $< ../output/construction_condominium_history.csv $@ pin year

include ../../shared/code/generic.make
