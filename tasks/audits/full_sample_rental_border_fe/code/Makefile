SHELL := bash

all:

full_sample_tables: \
	../output/fe_table_rental_bwall_pre_2023_all.csv \
	../output/fe_table_rental_bwall_pre_2023_all.tex \
	../output/year_diagnostics_bwall_pre_2023_all.csv \
	../output/fe_table_rental_bwall_pre_2023_multifamily_only.csv \
	../output/fe_table_rental_bwall_pre_2023_multifamily_only.tex \
	../output/year_diagnostics_bwall_pre_2023_multifamily_only.csv \
	../output/fe_table_rental_bw500_pre_2023_multifamily_only.csv \
	../output/fe_table_rental_bw500_pre_2023_multifamily_only.tex \
	../output/year_diagnostics_bw500_pre_2023_multifamily_only.csv

../output/fe_table_rental_bwall_pre_2023_all.csv: ../../rental_border_pair_fe_audit/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript ../../rental_border_pair_fe_audit/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet all pre_2023 all ../output/fe_table_rental_bwall_pre_2023_all.tex ../output/fe_table_rental_bwall_pre_2023_all.csv ../output/year_diagnostics_bwall_pre_2023_all.csv segment segment

../output/fe_table_rental_bwall_pre_2023_all.tex: ../output/fe_table_rental_bwall_pre_2023_all.csv
	@test -s "$@"

../output/year_diagnostics_bwall_pre_2023_all.csv: ../output/fe_table_rental_bwall_pre_2023_all.csv
	@test -s "$@"

../output/fe_table_rental_bwall_pre_2023_multifamily_only.csv: ../../rental_border_pair_fe_audit/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript ../../rental_border_pair_fe_audit/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet all pre_2023 multifamily_only ../output/fe_table_rental_bwall_pre_2023_multifamily_only.tex ../output/fe_table_rental_bwall_pre_2023_multifamily_only.csv ../output/year_diagnostics_bwall_pre_2023_multifamily_only.csv segment segment

../output/fe_table_rental_bwall_pre_2023_multifamily_only.tex: ../output/fe_table_rental_bwall_pre_2023_multifamily_only.csv
	@test -s "$@"

../output/year_diagnostics_bwall_pre_2023_multifamily_only.csv: ../output/fe_table_rental_bwall_pre_2023_multifamily_only.csv
	@test -s "$@"

../output/fe_table_rental_bw500_pre_2023_multifamily_only.csv: ../../rental_border_pair_fe_audit/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript ../../rental_border_pair_fe_audit/code/rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet 500 pre_2023 multifamily_only ../output/fe_table_rental_bw500_pre_2023_multifamily_only.tex ../output/fe_table_rental_bw500_pre_2023_multifamily_only.csv ../output/year_diagnostics_bw500_pre_2023_multifamily_only.csv segment segment

../output/fe_table_rental_bw500_pre_2023_multifamily_only.tex: ../output/fe_table_rental_bw500_pre_2023_multifamily_only.csv
	@test -s "$@"

../output/year_diagnostics_bw500_pre_2023_multifamily_only.csv: ../output/fe_table_rental_bw500_pre_2023_multifamily_only.csv
	@test -s "$@"

../input/rent_with_ward_distances.parquet: ../../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../../shell_functions.make
include ../../../generic.make
