SHELL := bash
include ../../shell_functions.make

INPUT_FILE := ../input/rent_with_ward_distances.parquet

COMPOSITION_TEX := ../output/composition_checks_pre_2021_all_bw500.tex
COMPOSITION_CSV := ../output/composition_checks_pre_2021_all_bw500.csv
PLACEBO_CSV := ../output/placebo_shifted_pre_2021_all_bw250.csv
PLACEBO_PDF := ../output/placebo_shifted_pre_2021_all_bw250.pdf
PLACEBO_BW500_CSV := ../output/placebo_shifted_pre_2021_all_bw500_shift750.csv
PLACEBO_BW500_PDF := ../output/placebo_shifted_pre_2021_all_bw500_shift750.pdf
DECAY_CSV := ../output/distance_decay_pre_2021_all.csv
DECAY_PDF := ../output/distance_decay_pre_2021_all.pdf
VISUAL_CSV := ../output/shifted_cutoff_visuals_pre_2021_all_bw250_shift750.csv
VISUAL_BINS_CSV := ../output/shifted_cutoff_visuals_bins_pre_2021_all_bw250_shift750.csv
VISUAL_PDF := ../output/shifted_cutoff_visuals_pre_2021_all_bw250_shift750.pdf
VISUAL_BW500_CSV := ../output/shifted_cutoff_visuals_pre_2021_all_bw500_shift750.csv
VISUAL_BW500_BINS_CSV := ../output/shifted_cutoff_visuals_bins_pre_2021_all_bw500_shift750.csv
VISUAL_BW500_PDF := ../output/shifted_cutoff_visuals_pre_2021_all_bw500_shift750.pdf

all: $(COMPOSITION_TEX) $(COMPOSITION_CSV) $(PLACEBO_CSV) $(PLACEBO_PDF) $(PLACEBO_BW500_CSV) $(PLACEBO_BW500_PDF) $(DECAY_CSV) $(DECAY_PDF) $(VISUAL_CSV) $(VISUAL_BINS_CSV) $(VISUAL_PDF) $(VISUAL_BW500_CSV) $(VISUAL_BW500_BINS_CSV) $(VISUAL_BW500_PDF)

$(COMPOSITION_TEX) $(COMPOSITION_CSV): run_composition_checks.R $(INPUT_FILE) | ../output
	Rscript run_composition_checks.R --input=$(INPUT_FILE) --bw_ft=500 --window=pre_2021 --sample_filter=all --output_tex=$(COMPOSITION_TEX) --output_csv=$(COMPOSITION_CSV)

$(PLACEBO_CSV) $(PLACEBO_PDF): run_placebo_shifted_boundary.R $(INPUT_FILE) | ../output
	Rscript run_placebo_shifted_boundary.R --input=$(INPUT_FILE) --bw_ft=250 --window=pre_2021 --sample_filter=all --shifts="-750,750" --use_controls=TRUE --output_csv=$(PLACEBO_CSV) --output_pdf=$(PLACEBO_PDF)

$(PLACEBO_BW500_CSV) $(PLACEBO_BW500_PDF): run_placebo_shifted_boundary.R $(INPUT_FILE) | ../output
	Rscript run_placebo_shifted_boundary.R --input=$(INPUT_FILE) --bw_ft=500 --window=pre_2021 --sample_filter=all --shifts="-750,750" --use_controls=TRUE --output_csv=$(PLACEBO_BW500_CSV) --output_pdf=$(PLACEBO_BW500_PDF)

$(DECAY_CSV) $(DECAY_PDF): run_distance_decay.R $(INPUT_FILE) | ../output
	Rscript run_distance_decay.R --input=$(INPUT_FILE) --window=pre_2021 --sample_filter=all --rings="0-250,250-500,500-1000,1000-1500" --use_controls=TRUE --output_csv=$(DECAY_CSV) --output_pdf=$(DECAY_PDF)

$(VISUAL_CSV) $(VISUAL_BINS_CSV) $(VISUAL_PDF): run_shifted_cutoff_visuals.R $(INPUT_FILE) | ../output
	Rscript run_shifted_cutoff_visuals.R --input=$(INPUT_FILE) --bw_ft=250 --window=pre_2021 --sample_filter=all --shifts="-750,0,750" --use_controls=TRUE --bins_per_side=6 --output_csv=$(VISUAL_CSV) --output_bins_csv=$(VISUAL_BINS_CSV) --output_pdf=$(VISUAL_PDF)

$(VISUAL_BW500_CSV) $(VISUAL_BW500_BINS_CSV) $(VISUAL_BW500_PDF): run_shifted_cutoff_visuals.R $(INPUT_FILE) | ../output
	Rscript run_shifted_cutoff_visuals.R --input=$(INPUT_FILE) --bw_ft=500 --window=pre_2021 --sample_filter=all --shifts="-750,0,750" --use_controls=TRUE --bins_per_side=8 --output_csv=$(VISUAL_BW500_CSV) --output_bins_csv=$(VISUAL_BW500_BINS_CSV) --output_pdf=$(VISUAL_BW500_PDF)

$(INPUT_FILE): ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT_FILE)

include ../../generic.make
