SHELL := bash

BANDWIDTH_FT ?= 500

all: \
	../output/rental_rd_rent_attenuation_clean_location_bw$(BANDWIDTH_FT).tex

../output/rental_rd_rent_attenuation_clean_location_bw$(BANDWIDTH_FT).tex: render_rental_rd_rent_attenuation_table.R ../temp/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).csv | ../output
	Rscript render_rental_rd_rent_attenuation_table.R $(BANDWIDTH_FT) clean_location

../temp/rental_rd_rent_attenuation_bw$(BANDWIDTH_FT).csv: build_rental_rd_rent_attenuation.R ../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet | ../temp
	Rscript build_rental_rd_rent_attenuation.R $(BANDWIDTH_FT)

../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet

include ../../generic.make
