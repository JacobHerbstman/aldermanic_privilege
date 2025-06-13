SHELL := bash
include ../../shell_functions.make

#───────────────────────────────────────────────────────────────────────────────
# Default goal: the CSV you actually want
#───────────────────────────────────────────────────────────────────────────────
.DEFAULT_GOAL := ../output/chi_addresses_for_geocoding.csv
.DELETE_ON_ERROR:

#───────────────────────────────────────────────────────────────────────────────
# 1) Populate ../input by symlinking upstream outputs
#───────────────────────────────────────────────────────────────────────────────
.PHONY: setup_input
setup_input: | ../input
	@echo "→ Refreshing input symlinks from upstream tasks…"
	rm -rf ../input/*
	ln -sf ../../process_attom_assessor_historical/output/* ../input/

#───────────────────────────────────────────────────────────────────────────────
# 2) Build the CSV, then move it to data_link and relink into ../output
#───────────────────────────────────────────────────────────────────────────────
../output/chi_addresses_for_geocoding.csv: \
        geocode_assessor_history.R \
        | setup_input ../output
	@echo "→ Running geocode script…"
	Rscript $<
	@echo "→ Moving result into central storage…"
	mkdir -p ../../data_link/geocode_assessor_history_output
	mv   ../output/chi_addresses_for_geocoding.csv \
	     ../../data_link/geocode_assessor_history_output/
	@echo "→ Symlinking it back into this task’s output folder…"
	ln -sf ../../data_link/geocode_assessor_history_output/chi_addresses_for_geocoding.csv \
	       ../output/chi_addresses_for_geocoding.csv

#───────────────────────────────────────────────────────────────────────────────
# 3) Generic helpers (creates ../input, ../output, ../temp, slurmlogs)
#───────────────────────────────────────────────────────────────────────────────
include ../../generic.make
