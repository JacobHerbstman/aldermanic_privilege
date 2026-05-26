SHELL := bash

all: ../output/level_plot_bw500_pct0_ctrl_clust_ward_pair.pdf

../output/level_plot_bw500_pct0_ctrl_clust_ward_pair.pdf: rental_level_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_level_plot.R

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../shell_functions.make
include ../../generic.make
