SHELL := bash
include ../../shell_functions.make

all: ../output/score_comparison_summary.csv ../output/score_outcome_regressions.csv ../output/main_outcome_score_summary.csv

../output/score_comparison_summary.csv: verify_strictness_scores.R ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/alderman_uncertainty_index_ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE_2stage.csv | ../output
	$(R) verify_strictness_scores.R

../output/score_outcome_regressions.csv: compare_scores_regressions.R ../output/score_comparison_summary.csv ../input/chicago_alderman_panel.csv | ../output
	$(R) compare_scores_regressions.R

../output/main_outcome_score_summary.csv: run_main_outcome_score_scan.R ../input/parcels_pre_scores.csv ../input/chicago_alderman_panel.csv ../input/block_treatment_pre_scores.csv ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/rental_listing_panel.parquet ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/alderman_restrictiveness_scores_ward_month_FEs.csv ../input/alderman_uncertainty_index_ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE_2stage.csv | ../output
	$(R) run_main_outcome_score_scan.R

../input/building_permits_text_features.csv.gz: ../../building_permits_scraping/output/building_permits_text_features.csv.gz | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/alderman_restrictiveness_scores_month_FEs.csv: ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

../input/alderman_restrictiveness_scores_ward_month_FEs.csv: ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_ward_month_FEs.csv | ../input
	ln -sf $< $@

../input/alderman_uncertainty_index_ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE_2stage.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE_2stage.csv | ../input
	ln -sf $< $@

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf $< $@

../input/block_treatment_pre_scores.csv: ../../create_block_treatment_panel/output/block_treatment_pre_scores.csv | ../input
	ln -sf $< $@

../input/sales_transaction_panel_2012.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet | ../input
	ln -sf $< $@

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf $< $@

../input/rental_listing_panel.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/alderman_restrictiveness_scores_ward_month_FEs.csv ../input/alderman_uncertainty_index_ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE_2stage.csv ../input/parcels_pre_scores.csv ../input/block_treatment_pre_scores.csv ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/rental_listing_panel.parquet
