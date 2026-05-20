SHELL := bash

BANDWIDTH_FT ?= 500
BINS_PER_SIDE ?= 15

all: \
	../output/rental_rd_supply_side_panel_bw$(BANDWIDTH_FT).parquet \
	../output/rental_rd_supply_side_means_bw$(BANDWIDTH_FT).csv \
	../output/rental_rd_supply_side_means_bw$(BANDWIDTH_FT).pdf \
	../output/rental_rd_supply_ppml_bw$(BANDWIDTH_FT).csv \
	../output/rental_rd_supply_ppml_bw$(BANDWIDTH_FT).pdf \
	../output/rental_rd_supply_bins_bw$(BANDWIDTH_FT).csv \
	../output/rental_rd_supply_bins_bw$(BANDWIDTH_FT).pdf \
	../output/rental_rd_supply_levels_bw$(BANDWIDTH_FT).pdf \
	../output/rental_rd_supply_levels_clean_location_bw$(BANDWIDTH_FT).pdf \
	../output/rental_rd_supply_sample_summary_bw$(BANDWIDTH_FT).csv

../output/rental_rd_supply_side_panel_bw$(BANDWIDTH_FT).parquet: build_rental_rd_supply.R ../input/rent_with_ward_distances.parquet | ../output
	$(R) $< $(BANDWIDTH_FT) $(BINS_PER_SIDE)

../output/rental_rd_supply_side_means_bw$(BANDWIDTH_FT).csv \
../output/rental_rd_supply_side_means_bw$(BANDWIDTH_FT).pdf \
../output/rental_rd_supply_ppml_bw$(BANDWIDTH_FT).csv \
../output/rental_rd_supply_ppml_bw$(BANDWIDTH_FT).pdf \
../output/rental_rd_supply_bins_bw$(BANDWIDTH_FT).csv \
../output/rental_rd_supply_bins_bw$(BANDWIDTH_FT).pdf \
../output/rental_rd_supply_levels_bw$(BANDWIDTH_FT).pdf \
../output/rental_rd_supply_levels_clean_location_bw$(BANDWIDTH_FT).pdf \
../output/rental_rd_supply_sample_summary_bw$(BANDWIDTH_FT).csv: ../output/rental_rd_supply_side_panel_bw$(BANDWIDTH_FT).parquet
	@test -s "$@"

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../shell_functions.make
include ../../generic.make
