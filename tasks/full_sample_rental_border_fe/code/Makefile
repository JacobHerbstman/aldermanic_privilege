SHELL := bash
include ../../shell_functions.make

all: \
	../output/fe_table_rental_bwall_pre_2023_all.csv \
	../output/fe_table_rental_bwall_pre_2023_all.tex \
	../output/year_diagnostics_bwall_pre_2023_all.csv \
	../output/fe_table_rental_bwall_pre_2023_multifamily_only.csv \
	../output/fe_table_rental_bwall_pre_2023_multifamily_only.tex \
	../output/year_diagnostics_bwall_pre_2023_multifamily_only.csv \
	../output/fe_table_rental_bw500_pre_2023_multifamily_only.csv \
	../output/fe_table_rental_bw500_pre_2023_multifamily_only.tex \
	../output/year_diagnostics_bw500_pre_2023_multifamily_only.csv

../output/fe_table_rental_bwall_pre_2023_all.csv: ../../rental_border_pair_fe/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript ../../rental_border_pair_fe/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet all pre_2023 all ../output/fe_table_rental_bwall_pre_2023_all.tex ../output/fe_table_rental_bwall_pre_2023_all.csv ../output/year_diagnostics_bwall_pre_2023_all.csv segment segment

../output/fe_table_rental_bwall_pre_2023_all.tex: ../output/fe_table_rental_bwall_pre_2023_all.csv
	test -f "$@"

../output/year_diagnostics_bwall_pre_2023_all.csv: ../output/fe_table_rental_bwall_pre_2023_all.csv
	test -f "$@"

../output/fe_table_rental_bwall_pre_2023_multifamily_only.csv: ../../rental_border_pair_fe/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript ../../rental_border_pair_fe/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet all pre_2023 multifamily_only ../output/fe_table_rental_bwall_pre_2023_multifamily_only.tex ../output/fe_table_rental_bwall_pre_2023_multifamily_only.csv ../output/year_diagnostics_bwall_pre_2023_multifamily_only.csv segment segment

../output/fe_table_rental_bwall_pre_2023_multifamily_only.tex: ../output/fe_table_rental_bwall_pre_2023_multifamily_only.csv
	test -f "$@"

../output/year_diagnostics_bwall_pre_2023_multifamily_only.csv: ../output/fe_table_rental_bwall_pre_2023_multifamily_only.csv
	test -f "$@"

../output/fe_table_rental_bw500_pre_2023_multifamily_only.csv: ../../rental_border_pair_fe/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript ../../rental_border_pair_fe/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet 500 pre_2023 multifamily_only ../output/fe_table_rental_bw500_pre_2023_multifamily_only.tex ../output/fe_table_rental_bw500_pre_2023_multifamily_only.csv ../output/year_diagnostics_bw500_pre_2023_multifamily_only.csv segment segment

../output/fe_table_rental_bw500_pre_2023_multifamily_only.tex: ../output/fe_table_rental_bw500_pre_2023_multifamily_only.csv
	test -f "$@"

../output/year_diagnostics_bw500_pre_2023_multifamily_only.csv: ../output/fe_table_rental_bw500_pre_2023_multifamily_only.csv
	test -f "$@"

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../generic.make
