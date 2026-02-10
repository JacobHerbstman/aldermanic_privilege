SHELL := bash
include ../../shell_functions.make

INPUT := ../input/rent_with_ward_distances.parquet

FE_TEX := ../output/fe_table_rental_bw500_pre_2023.tex
LEVEL_PCT0 := ../output/level_plot_bw500_pct0_ctrl.pdf
LEVEL_PCT50 := ../output/level_plot_bw500_pct50_ctrl.pdf

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(FE_TEX) $(LEVEL_PCT0) $(LEVEL_PCT50)

$(FE_TEX): rental_border_pair_fe.R $(INPUT) | ../output
	Rscript rental_border_pair_fe.R --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--output_tex="$@" \
		--output_csv="../output/fe_table_rental_bw500_pre_2023.csv" \
		--output_year_diag="../output/year_diagnostics_bw500_pre_2023.csv"

$(LEVEL_PCT0): rental_level_plot.R $(INPUT) | ../output
	Rscript rental_level_plot.R --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--use_controls=TRUE --bins_per_side=15 --min_strictness_diff_pctile=0 \
		--output_pdf="$@"

$(LEVEL_PCT50): rental_level_plot.R $(INPUT) | ../output
	Rscript rental_level_plot.R --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--use_controls=TRUE --bins_per_side=15 --min_strictness_diff_pctile=50 \
		--output_pdf="$@"

$(INPUT): ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT)

include ../../generic.make
