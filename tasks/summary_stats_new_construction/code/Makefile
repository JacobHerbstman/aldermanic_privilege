SHELL := bash
include ../../shell_functions.make

all: ../output/summary_stats.tex ../output/summary_stats_tabular.tex

../output/summary_stats.tex: summary_stats.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript summary_stats.R ../input/parcels_with_ward_distances.csv ../output/summary_stats.tex ../output/summary_stats_tabular.tex

../output/summary_stats_tabular.tex: ../output/summary_stats.tex
	test -f "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
