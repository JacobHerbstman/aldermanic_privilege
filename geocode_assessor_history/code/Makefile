SHELL := bash
include ../../generic.make

all: ../output/chi_addresses_for_geocoding.csv

.PHONY: setup_input
setup_input: | ../input
	@echo "→ Refreshing input symlinks…"
	@rm -rf ../input/*
	@ln -sf ../../process_attom_assessor_historical/output/* ../input/
	@ln -sf ../../merge_wards_tracts/output/*       ../input/

../output/chi_addresses_for_geocoding.csv: \
    geocode_assessor_history.R | setup_input ../output
	@echo "→ Generating chi_addresses_for_geocoding.csv…"
	Rscript $<

