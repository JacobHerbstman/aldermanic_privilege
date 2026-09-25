include ../../shared/code/shell_functions.make

all: ../output/sales_with_ward_distances_through2022.csv

../output/sales_with_ward_distances_through2022.csv: merge_sales_scores.R ../input/sales_pre_scores.csv ../input/aldermen_uncertainty_scores_through2022.csv Makefile | ../output ../report
	$(R) $<

../input/sales_pre_scores.csv: ../../assign_segment_ids_sales_rental/output/sales_pre_scores_with_segments.csv | ../input
	ln -sf $< $@

../input/aldermen_uncertainty_scores_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@

link-inputs: ../input/sales_pre_scores.csv ../input/aldermen_uncertainty_scores_through2022.csv

include ../../shared/code/generic.make
