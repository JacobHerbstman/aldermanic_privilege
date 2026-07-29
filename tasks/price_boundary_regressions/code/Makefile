BANDWIDTH_FT := 500
RENT_START_YEAR := 2014
RENT_END_YEAR := 2022
SALES_START_YEAR := 2006
SALES_END_YEAR := 2022
BINS_PER_SIDE := 10

all: ../output/price_boundary_regressions_$(BANDWIDTH_FT)ft.tex \
	../output/price_boundary_rent_$(BANDWIDTH_FT)ft.pdf \
	../output/price_boundary_sales_$(BANDWIDTH_FT)ft.pdf

../output/price_boundary_regressions_$(BANDWIDTH_FT)ft.tex: \
	build_price_boundary_regressions.R \
	../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet \
	../input/sales_with_hedonics_amenities.parquet | ../output
	$(R) $< $(BANDWIDTH_FT) $(BINS_PER_SIDE) $(RENT_START_YEAR) $(RENT_END_YEAR) $(SALES_START_YEAR) $(SALES_END_YEAR)

../output/price_boundary_rent_$(BANDWIDTH_FT)ft.pdf: \
		../output/price_boundary_regressions_$(BANDWIDTH_FT)ft.tex
	@test -s "$@"

../output/price_boundary_sales_$(BANDWIDTH_FT)ft.pdf: \
		../output/price_boundary_regressions_$(BANDWIDTH_FT)ft.tex
	@test -s "$@"

../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw$(BANDWIDTH_FT).parquet \
	../input/sales_with_hedonics_amenities.parquet

include ../../shell_functions.make
include ../../generic.make
