SHELL := bash
include ../../shell_functions.make

INPUT := ../input/sales_with_hedonics.parquet
FE_TEX := ../output/fe_table_sales_bw1000_year_quarter.tex

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(FE_TEX)

$(FE_TEX): sales_border_pair_fe.R $(INPUT) | ../output
	Rscript sales_border_pair_fe.R --bw_ft=1000 --fe_time=year_quarter \
		--output_tex="$@" \
		--output_csv="../output/fe_table_sales_bw1000_year_quarter.csv" \
		--output_year_diag="../output/year_diagnostics_sales_bw1000_year_quarter.csv"

$(INPUT): ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT)

include ../../generic.make
