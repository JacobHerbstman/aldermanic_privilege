SHELL := bash

BANDWIDTH_FT ?= 500
BINS_PER_SIDE ?= 15

all: ../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_all_controls.pdf

../output/rental_rd_flat_bw$(BANDWIDTH_FT)_2014_2022_all_controls.pdf: run_rental_rd.R ../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet | ../output
	$(R) $< $(BANDWIDTH_FT) all TRUE $(BINS_PER_SIDE)

../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet

include ../../shell_functions.make
include ../../generic.make
