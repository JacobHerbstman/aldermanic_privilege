SHELL := bash
include ../../shell_functions.make

INPUT := ../input/rent_with_ward_distances.parquet

CHAR_TEX := ../output/char_fe_table_bw500.tex
LISTING_PDF := ../output/listing_density_bw500_pre_2023_all_pct0.pdf

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(CHAR_TEX) $(LISTING_PDF)

$(CHAR_TEX): characteristic_fe_table.R $(INPUT) | ../output
	Rscript characteristic_fe_table.R --bw_ft=500 --window=pre_2023 \
		--output_tex="$@" --output_csv="../output/char_fe_table_bw500.csv"

$(LISTING_PDF): listing_density_plot.R $(INPUT) | ../output
	Rscript listing_density_plot.R --bw_ft=500 --window=pre_2023 --sample_filter=all \
		--min_strictness_diff_pctile=0 --bins_per_side=15 --output_pdf="$@"

$(INPUT): ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT)

include ../../generic.make
