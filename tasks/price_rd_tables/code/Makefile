BANDWIDTH_FT ?= 500

all: ../output/price_rd_controls_panels_bw$(BANDWIDTH_FT).tex

../output/price_rd_controls_panels_bw$(BANDWIDTH_FT).tex: build_price_rd_controls_table.R ../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet ../input/sales_with_hedonics_amenities.parquet | ../output
	Rscript $< $(BANDWIDTH_FT)

../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet \
	../input/sales_with_hedonics_amenities.parquet

include ../../generic.make
