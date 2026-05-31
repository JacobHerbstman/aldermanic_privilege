SCORE_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through202604
SCORE_COLUMN ?= uncertainty_index

all: ../output/sales_with_ward_distances.csv ../output/block_treatment_panel.csv ../output/rent_with_ward_distances_full.parquet

../output/sales_with_ward_distances.csv: merge_sales_treatment_scores.R ../input/sales_pre_scores.csv ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores.csv | ../output
	Rscript $< $(SCORE_COLUMN)

../output/block_treatment_panel.csv: ../output/sales_with_ward_distances.csv
	@test -s "$@"

../output/rent_with_ward_distances_full.parquet: merge_rent_scores.R ../input/rent_pre_scores_full.parquet ../input/aldermen_uncertainty_scores.csv | ../output
	Rscript $< $(SCORE_COLUMN)
	@test -s "$@"

../input/sales_pre_scores.csv: ../../assign_segment_ids_sales_rental/output/sales_pre_scores_with_segments.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rent_pre_scores_full.parquet: ../../assign_segment_ids_sales_rental/output/rent_pre_scores_full_with_segments.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/block_treatment_pre_scores.csv: ../../create_block_treatment_panel/output/block_treatment_pre_scores.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(SCORE_SPEC).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores.csv

include ../../generic.make
