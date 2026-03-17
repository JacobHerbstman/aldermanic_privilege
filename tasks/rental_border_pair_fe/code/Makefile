SHELL := bash
include ../../shell_functions.make

all: \
	../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex \
	../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.csv \
	../output/year_diagnostics_bw500_pre_2023_clust_ward_pair.csv \
	../output/level_plot_bw500_pct0_ctrl_clust_ward_pair.pdf

../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex: rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all "$@" ../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.csv ../output/year_diagnostics_bw500_pre_2023_clust_ward_pair.csv segment ward_pair

../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.csv: ../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex
	test -f "$@"

../output/year_diagnostics_bw500_pre_2023_clust_ward_pair.csv: ../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex
	test -f "$@"

../output/level_plot_bw500_pct0_ctrl_clust_ward_pair.pdf: rental_level_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_level_plot.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all TRUE 15 0 "$@" NA NA segment ward_pair

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../generic.make
