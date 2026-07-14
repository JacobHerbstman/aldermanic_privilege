all: ../output/sales_with_ward_distances_through2022.csv ../output/block_treatment_panel_through202604.csv ../output/rent_with_ward_distances_full_through2022.parquet

../output/sales_with_ward_distances_through2022.csv: merge_sales_treatment_scores.R ../input/sales_pre_scores.csv ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores_through2022.csv ../input/aldermen_uncertainty_scores_through202604.csv | ../output
	Rscript $<

../output/block_treatment_panel_through202604.csv: ../output/sales_with_ward_distances_through2022.csv
	@test -s "$@"

../output/rent_with_ward_distances_full_through2022.parquet: merge_rent_scores.R ../input/rent_pre_scores_full.parquet ../input/aldermen_uncertainty_scores_through2022.csv | ../output
	Rscript $<
	@test -s "$@"

../input/sales_pre_scores.csv: ../../assign_segment_ids_sales_rental/output/sales_pre_scores_with_segments.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rent_pre_scores_full.parquet: ../../assign_segment_ids_sales_rental/output/rent_pre_scores_full_with_segments.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/block_treatment_pre_scores.csv: ../../create_block_treatment_panel/output/block_treatment_pre_scores.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores_through202604.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through202604.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores_through2022.csv ../input/aldermen_uncertainty_scores_through202604.csv

include ../../generic.make
