SHELL := bash
include ../../shell_functions.make

INPUT := ../input/rent_with_ward_distances.parquet

CHAR_TEX := ../output/char_fe_table_bw500.tex
LISTING_PDF := ../output/listing_density_bw500_pre_2023_all_pct0.pdf
LISTING_UNITS_PDF := ../output/listing_units_bw500_pre_2023_all_pct0.pdf
LISTING_UNITS_TEX := ../output/listing_units_fe_table_bw500_pre_2023_all_pct0.tex
LISTING_UNITS_CSV := ../output/listing_units_fe_table_bw500_pre_2023_all_pct0.csv
LISTING_UNITS_PPML_TEX := ../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.tex
LISTING_UNITS_PPML_CSV := ../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.csv
LISTING_UNITS_PPML_ROBUST_TEX := ../output/listing_units_ppml_robustness_bw500_pre_2023.tex
LISTING_UNITS_PPML_ROBUST_CSV := ../output/listing_units_ppml_robustness_bw500_pre_2023.csv
LISTING_UNITS_PPML_MAIN_TEX := ../output/listing_units_ppml_main_bw500_pre_2023.tex
LISTING_UNITS_PPML_MAIN_CSV := ../output/listing_units_ppml_main_bw500_pre_2023.csv
SIDE_COMP_PDF := ../output/listing_units_side_comparison_bw500_pre_2023.pdf
MATCHED_RATIO_PDF := ../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.pdf
MATCHED_RATIO_CSV := ../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.csv

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(CHAR_TEX) $(LISTING_PDF) $(LISTING_UNITS_PDF) $(SIDE_COMP_PDF) $(MATCHED_RATIO_PDF) $(MATCHED_RATIO_CSV) $(LISTING_UNITS_TEX) $(LISTING_UNITS_CSV) $(LISTING_UNITS_PPML_TEX) $(LISTING_UNITS_PPML_CSV) $(LISTING_UNITS_PPML_ROBUST_TEX) $(LISTING_UNITS_PPML_ROBUST_CSV) $(LISTING_UNITS_PPML_MAIN_TEX) $(LISTING_UNITS_PPML_MAIN_CSV)

$(CHAR_TEX): characteristic_fe_table.R $(INPUT) | ../output
	Rscript characteristic_fe_table.R --bw_ft=500 --window=pre_2023 \
		--output_tex="$@" --output_csv="../output/char_fe_table_bw500.csv"

$(LISTING_PDF): listing_density_plot.R $(INPUT) | ../output
	Rscript listing_density_plot.R --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--min_strictness_diff_pctile=0 --bins_per_side=15 --output_pdf="$@"

$(LISTING_UNITS_PDF): listing_units_plot.R $(INPUT) | ../output
	Rscript listing_units_plot.R --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--unit_def=unit_proxy --min_strictness_diff_pctile=0 --bins_per_side=8 \
		--output_pdf="$@"

$(SIDE_COMP_PDF): listing_units_side_comparison.R $(INPUT) | ../output
	Rscript listing_units_side_comparison.R --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--unit_def=unit_proxy --min_strictness_diff_pctile=0 --output_pdf="$@"

$(MATCHED_RATIO_PDF): listing_units_matched_pair_ratio_plot.R $(INPUT) | ../output
	Rscript listing_units_matched_pair_ratio_plot.R --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--unit_def=unit_proxy --min_strictness_diff_pctile=0 --output_pdf="$@" \
		--output_csv="$(MATCHED_RATIO_CSV)"

$(MATCHED_RATIO_CSV): $(MATCHED_RATIO_PDF)

$(LISTING_UNITS_TEX): listing_units_fe_table.R $(INPUT) | ../output
	Rscript listing_units_fe_table.R --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--min_strictness_diff_pctile=0 --output_tex="$@" --output_csv="$(LISTING_UNITS_CSV)"

$(LISTING_UNITS_CSV): $(LISTING_UNITS_TEX)

$(LISTING_UNITS_PPML_TEX): listing_units_ppml_fe_table.R $(INPUT) | ../output
	Rscript listing_units_ppml_fe_table.R --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--min_strictness_diff_pctile=0 --unit_def=unit_proxy \
		--output_tex="$@" --output_csv="$(LISTING_UNITS_PPML_CSV)"

$(LISTING_UNITS_PPML_CSV): $(LISTING_UNITS_PPML_TEX)

$(LISTING_UNITS_PPML_ROBUST_TEX): listing_units_ppml_robustness.R $(INPUT) | ../output
	Rscript listing_units_ppml_robustness.R --bw_ft=500 --window=pre_2023 \
		--output_tex="$@" --output_csv="$(LISTING_UNITS_PPML_ROBUST_CSV)"

$(LISTING_UNITS_PPML_ROBUST_CSV): $(LISTING_UNITS_PPML_ROBUST_TEX)

$(LISTING_UNITS_PPML_MAIN_TEX): listing_units_ppml_main_table.R $(INPUT) | ../output
	Rscript listing_units_ppml_main_table.R --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--unit_def=unit_proxy --output_tex="$@" --output_csv="$(LISTING_UNITS_PPML_MAIN_CSV)"

$(LISTING_UNITS_PPML_MAIN_CSV): $(LISTING_UNITS_PPML_MAIN_TEX)

$(INPUT): ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT)

include ../../generic.make
