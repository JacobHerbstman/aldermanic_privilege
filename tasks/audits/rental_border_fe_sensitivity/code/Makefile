all: ../output/permutation_test_2014_2022_all_bw500_ctrl_clust_ward_pair.pdf

../output/permutation_test_2014_2022_all_bw500_ctrl_clust_ward_pair.pdf: run_permutation_test.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript $<

../input/rent_with_ward_distances.parquet: ../../../merge_rent_scores/output/rent_with_ward_distances_full_through2022.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet

include ../../../generic.make
