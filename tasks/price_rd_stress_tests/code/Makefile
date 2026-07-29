BANDWIDTH_FT ?= 500
BINS_PER_SIDE ?= 10
PLACEBO_CUTOFF_FT ?= 1000

all: \
	../output/rent_placebo_rd_main_style.pdf \
	../output/sales_placebo_rd_main_style.pdf

../output/rent_placebo_rd_main_style.pdf: build_price_rd_main_style_plots.R ../input/rental_rd_characteristics_panel_bw1500.parquet | ../output
	Rscript $< rent $(BANDWIDTH_FT) $(BINS_PER_SIDE) $(PLACEBO_CUTOFF_FT)

../output/sales_placebo_rd_main_style.pdf: build_price_rd_main_style_plots.R ../input/sales_with_hedonics_amenities.parquet | ../output
	Rscript $< sales $(BANDWIDTH_FT) $(BINS_PER_SIDE) $(PLACEBO_CUTOFF_FT)

../input/sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rental_rd_characteristics_panel_bw1500.parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw1500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw1500.parquet \
	../input/sales_with_hedonics_amenities.parquet

include ../../generic.make
