include ../../shared/code/shell_functions.make

START_YEAR ?= 1999
END_YEAR ?= 2026

all: ../output/construction_condominium_history.csv

download-current: ../output/residential_improvement_characteristics_full_current.csv ../output/commercial_valuation_data_current.csv ../output/construction_condominium_history_current.csv

../output/residential_improvement_characteristics_full_current.csv: ../../download_residential_improvements_full/code/download_residential_improvements.sh Makefile | ../output
	bash $< $(START_YEAR) $(END_YEAR)

../output/commercial_valuation_data_current.csv: download_commercial_valuation_data.sh | ../output
	bash $<

../output/construction_condominium_history.csv: ../../../data_raw/construction_review/construction_condominium_history_20260909.csv condominium_snapshot.sha256 Makefile | ../output
	shasum -a 256 -c condominium_snapshot.sha256
	cp $< $@.tmp
	mv $@.tmp $@

../output/construction_condominium_history_current.csv: download_construction_condos.R construction_condominium_queries.csv ../../setup_environment/code/packages.R | ../output ../temp
	$(R) $<

.PHONY: download-current

include ../../shared/code/generic.make
