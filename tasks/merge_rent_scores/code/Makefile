all: ../output/rent_with_ward_distances_full_through2022.parquet

../output/rent_with_ward_distances_full_through2022.parquet: merge_rent_scores.R ../input/rent_pre_scores_full.parquet ../input/aldermen_uncertainty_scores_through2022.csv | ../output
	Rscript $<

../input/rent_pre_scores_full.parquet: ../../assign_segment_ids_sales_rental/output/rent_pre_scores_full_with_segments.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_pre_scores_full.parquet ../input/aldermen_uncertainty_scores_through2022.csv

include ../../generic.make
