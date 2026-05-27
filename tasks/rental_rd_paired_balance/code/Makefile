SHELL := bash

BANDWIDTH_FT ?= 500

all: \
	../output/rental_rd_paired_external_amenity_balance_bw$(BANDWIDTH_FT).tex

../output/rental_rd_paired_external_amenity_balance_bw$(BANDWIDTH_FT).tex: build_rental_rd_paired_balance.R ../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet | ../output
	Rscript $< $(BANDWIDTH_FT)

../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet

include ../../generic.make
