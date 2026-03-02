SHELL := bash
include ../../shell_functions.make

all: \
	../output/char_fe_table_bw500.tex \
	../output/listing_density_bw500_pre_2023_all_pct0.pdf \
	../output/listing_units_unit_proxy_bw500_pre_2023_all_pct0.pdf \
	../output/listing_units_bw500_pre_2023_all_pct0.pdf \
	../output/listing_units_side_comparison_bw500_pre_2023.pdf \
	../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.pdf \
	../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.csv \
	../output/listing_units_fe_table_bw500_pre_2023_all_pct0.tex \
	../output/listing_units_fe_table_bw500_pre_2023_all_pct0.csv \
	../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.tex \
	../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.csv \
	../output/listing_units_ppml_robustness_bw500_pre_2023.tex \
	../output/listing_units_ppml_robustness_bw500_pre_2023.csv \
	../output/listing_units_ppml_main_bw500_pre_2023.tex \
	../output/listing_units_ppml_main_bw500_pre_2023.csv

../output/char_fe_table_bw500.tex: characteristic_fe_table.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript characteristic_fe_table.R ../input/rent_with_ward_distances.parquet 500 pre_2023 "$@" ../output/char_fe_table_bw500.csv

../output/listing_density_bw500_pre_2023_all_pct0.pdf: listing_density_plot.R ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv | ../output
	Rscript listing_density_plot.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all 0 15 "$@"

../output/listing_units_unit_proxy_bw500_pre_2023_all_pct0.pdf: listing_units_plot.R ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv | ../output
	Rscript listing_units_plot.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all unit_proxy 0 8 "$@"

../output/listing_units_bw500_pre_2023_all_pct0.pdf: ../output/listing_units_unit_proxy_bw500_pre_2023_all_pct0.pdf | ../output
	ln -sf "listing_units_unit_proxy_bw500_pre_2023_all_pct0.pdf" "$@"

../output/listing_units_side_comparison_bw500_pre_2023.pdf: listing_units_side_comparison.R ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv | ../output
	Rscript listing_units_side_comparison.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all unit_proxy 0 "$@"

../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.pdf: listing_units_matched_pair_ratio_plot.R ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv | ../output
	Rscript listing_units_matched_pair_ratio_plot.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all unit_proxy 0 "$@" ../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.csv

../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.csv: ../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.pdf

../output/listing_units_fe_table_bw500_pre_2023_all_pct0.tex: listing_units_fe_table.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript listing_units_fe_table.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all 0 "$@" ../output/listing_units_fe_table_bw500_pre_2023_all_pct0.csv

../output/listing_units_fe_table_bw500_pre_2023_all_pct0.csv: ../output/listing_units_fe_table_bw500_pre_2023_all_pct0.tex

../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.tex: listing_units_ppml_fe_table.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript listing_units_ppml_fe_table.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all 0 unit_proxy "$@" ../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.csv

../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.csv: ../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.tex

../output/listing_units_ppml_robustness_bw500_pre_2023.tex: listing_units_ppml_robustness.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript listing_units_ppml_robustness.R ../input/rent_with_ward_distances.parquet 500 pre_2023 "$@" ../output/listing_units_ppml_robustness_bw500_pre_2023.csv

../output/listing_units_ppml_robustness_bw500_pre_2023.csv: ../output/listing_units_ppml_robustness_bw500_pre_2023.tex

../output/listing_units_ppml_main_bw500_pre_2023.tex: listing_units_ppml_main_table.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript listing_units_ppml_main_table.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all unit_proxy "$@" ../output/listing_units_ppml_main_bw500_pre_2023.csv

../output/listing_units_ppml_main_bw500_pre_2023.csv: ../output/listing_units_ppml_main_bw500_pre_2023.tex

../output/listing_units_ppml_main_bw500_pre_2023_pruned.tex: listing_units_ppml_main_table.R ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv Rscript listing_units_ppml_main_table.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all unit_proxy "$@" ../output/listing_units_ppml_main_bw500_pre_2023_pruned.csv

../output/listing_units_ppml_main_bw500_pre_2023_pruned.csv: ../output/listing_units_ppml_main_bw500_pre_2023_pruned.tex

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv

include ../../generic.make
