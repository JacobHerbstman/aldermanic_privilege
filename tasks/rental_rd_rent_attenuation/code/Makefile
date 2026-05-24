SHELL := bash

BANDWIDTH_FT ?= 500

all: \
	../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).csv \
	../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).tex \
	../output/rental_rd_rent_attenuation_clean_location_bw$(BANDWIDTH_FT).tex \
	../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).pdf \
	../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).png

../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).csv: build_rental_rd_rent_attenuation.R ../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet | ../output
	$(R) $< $(BANDWIDTH_FT)

../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).tex \
../output/rental_rd_rent_attenuation_clean_location_bw$(BANDWIDTH_FT).tex \
../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).pdf \
../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).png: | ../output/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).csv
	@test -s "$@"

../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet

include ../../shell_functions.make
include ../../generic.make
