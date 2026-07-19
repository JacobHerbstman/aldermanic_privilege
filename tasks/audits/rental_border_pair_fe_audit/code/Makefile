SHELL := bash

all:

rental_fe_table: \
	../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex \
	../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.csv \
	../output/year_diagnostics_bw500_pre_2023_clust_ward_pair.csv

../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex: rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all "$@" ../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.csv ../output/year_diagnostics_bw500_pre_2023_clust_ward_pair.csv segment ward_pair

../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.csv: ../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex
	@test -s "$@"

../output/year_diagnostics_bw500_pre_2023_clust_ward_pair.csv: ../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex
	@test -s "$@"

../input/rent_with_ward_distances.parquet: ../../../merge_rent_scores/output/rent_with_ward_distances_full_through2022.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../../shell_functions.make
include ../../../generic.make
