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
	# arg order: input bw_ft window sample_filter output_tex output_csv output_year_diag
	Rscript rental_border_pair_fe.R "$(INPUT)" 500 pre_2023 all "$@" "../output/fe_table_rental_bw500_pre_2023.csv" "../output/year_diagnostics_bw500_pre_2023.csv"

$(LEVEL_PCT0): rental_level_plot.R $(INPUT) | ../output
	# arg order: input bw_ft window sample_filter use_controls bins_per_side min_strictness_diff_pctile output_pdf output_meta_csv output_bins_csv
	Rscript rental_level_plot.R "$(INPUT)" 500 pre_2023 all TRUE 15 0 "$@" NA NA

$(LEVEL_PCT50): rental_level_plot.R $(INPUT) | ../output
	# arg order: input bw_ft window sample_filter use_controls bins_per_side min_strictness_diff_pctile output_pdf output_meta_csv output_bins_csv
	Rscript rental_level_plot.R "$(INPUT)" 500 pre_2023 all TRUE 15 50 "$@" NA NA

$(INPUT): ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT)

include ../../generic.make
