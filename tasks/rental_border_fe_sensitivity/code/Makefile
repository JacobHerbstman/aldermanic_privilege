SHELL := bash
include ../../shell_functions.make

all: ../output/permutation_test_2014_2022_all_bw500_ctrl_clust_ward_pair.pdf

../output/permutation_test_2014_2022_all_bw500_ctrl.pdf: run_permutation_test.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript run_permutation_test.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all TRUE 500 42 2015-05-18 ../output/permutation_test_2014_2022_all_bw500_ctrl.csv $@

../output/permutation_test_2014_2022_all_bw500_ctrl_clust_ward_pair.pdf: run_permutation_test.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript run_permutation_test.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all TRUE 500 42 2015-05-18 ../output/permutation_test_2014_2022_all_bw500_ctrl_clust_ward_pair.csv $@

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../generic.make
