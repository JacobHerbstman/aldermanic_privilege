SHELL := bash

all: ../output/price_rd_controls_panels_bw500.tex

../output/price_rd_controls_panels_bw500.tex: build_price_rd_controls_table.R ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet | ../output
	Rscript $<

../output/price_rd_controls_panels_bw500.csv: ../output/price_rd_controls_panels_bw500.tex
	@test -s "$@"

../input/rental_rd_characteristics_panel_bw500.parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet

include ../../generic.make
