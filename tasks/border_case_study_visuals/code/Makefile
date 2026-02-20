SHELL := bash
include ../../shell_functions.make

all: ../output/case_study_far_panel_bw500.pdf ../output/case_study_summary_bw500.csv

../output/case_study_far_panel_bw500.pdf ../output/case_study_summary_bw500.csv: build_case_study_visuals.R ../input/parcels_with_ward_distances.csv | ../output
	# arg order: bw_ft borders bin_ft output_pdf output_csv
	Rscript build_case_study_visuals.R \
		500 \
		1_26,11_25 \
		50 \
		../output/case_study_far_panel_bw500.pdf \
		../output/case_study_summary_bw500.csv

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
