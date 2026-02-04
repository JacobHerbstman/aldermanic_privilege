SHELL := bash
include ../../shell_functions.make

SCORE_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage
SCORE_COLUMN ?= uncertainty_index

all: ../output/sales_with_ward_distances.csv ../output/rent_with_ward_distances_full.parquet ../output/block_treatment_panel.csv

../output/.merge_done.stamp: merge_event_study_scores.R ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores.csv | ../output
	$(R) merge_event_study_scores.R --score_file=../input/aldermen_uncertainty_scores.csv --score_column=$(SCORE_COLUMN) --sales_input=../input/sales_pre_scores.csv --rent_input=../input/rent_pre_scores_full.parquet --treatment_input=../input/block_treatment_pre_scores.csv --alderman_panel=../input/chicago_alderman_panel.csv --sales_output=../output/sales_with_ward_distances.csv --rent_output=../output/rent_with_ward_distances_full.parquet --treatment_output=../output/block_treatment_panel.csv
	touch $@

../output/sales_with_ward_distances.csv: ../output/.merge_done.stamp
../output/rent_with_ward_distances_full.parquet: ../output/.merge_done.stamp
../output/block_treatment_panel.csv: ../output/.merge_done.stamp

../input/sales_pre_scores.csv: ../../calculate_sale_distances/output/sales_pre_scores.csv | ../input
	ln -sf $< $@

../input/rent_pre_scores_full.parquet: ../../calculate_rent_distances/output/rent_pre_scores_full.parquet | ../input
	ln -sf $< $@

../input/block_treatment_pre_scores.csv: ../../create_block_treatment_panel/output/block_treatment_pre_scores.csv | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/aldermen_uncertainty_scores.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(SCORE_SPEC).csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores.csv
