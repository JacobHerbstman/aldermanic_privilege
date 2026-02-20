SHELL := bash
include ../../shell_functions.make

SCORE_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage
SCORE_COLUMN ?= uncertainty_index
SCORE_FILE_UPSTREAM ?= ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(SCORE_SPEC).csv

all: ../output/sales_with_ward_distances.csv ../output/rent_with_ward_distances_full.parquet ../output/block_treatment_panel.csv

../output/.merge_done.stamp: merge_event_study_scores.R ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores.csv | ../output
	# arg order: score_file score_column sales_input rent_input treatment_input alderman_panel sales_output rent_output treatment_output
	$(R) merge_event_study_scores.R ../input/aldermen_uncertainty_scores.csv $(SCORE_COLUMN) ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../output/sales_with_ward_distances.csv ../output/rent_with_ward_distances_full.parquet ../output/block_treatment_panel.csv
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

../input/aldermen_uncertainty_scores.csv: $(SCORE_FILE_UPSTREAM) | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/sales_pre_scores.csv ../input/rent_pre_scores_full.parquet ../input/block_treatment_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/aldermen_uncertainty_scores.csv

.PHONY: with-strictness

with-strictness:
	$(MAKE) SCORE_FILE_UPSTREAM=../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv SCORE_COLUMN=uncertainty_index
