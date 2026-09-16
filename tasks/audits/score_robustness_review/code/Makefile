include ../../../shared/code/shell_functions.make

START_YEAR := 2006
CUTOFF_YEARS := 2014,2022
MINIMUM_MONTHS := 4
STAGE1_CONTROLS := median_hh_income_10k + share_black + share_hisp + share_white + homeownership_rate + pop_total_10k + dist_cbd_km + dist_lake_km + n_rail_stations_800m + n_permits_wm_l1
STAGE1_FE := month + permit_type_clean + review_type_clean
RAW_MEASURES := mean_days,median_days,mean_log_days

all: ../output/score_robustness_review.html

../output/score_robustness_review.html ../output/robustness_summary.csv: build_robustness_review.R Makefile ../output/raw_processing_by_alderman.csv ../output/raw_processing_pairs.csv ../output/raw_processing_agreement.csv ../output/raw_processing_correlations.csv ../output/raw_processing_comparison.png ../input/multifamily_score_summary.csv ../input/multifamily_pair_sensitivity.csv ../input/permit_score_summary.csv ../input/permit_pair_sensitivity.csv ../input/price_score_summary.csv ../input/sales_pair_sensitivity.csv ../input/rent_pair_sensitivity.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $<

../output/raw_processing_by_alderman.csv ../output/raw_processing_pairs.csv ../output/raw_processing_agreement.csv ../output/raw_processing_correlations.csv ../output/raw_processing_comparison.pdf ../output/raw_processing_comparison.png: compare_raw_processing.R Makefile ../input/permits_for_uncertainty_index.csv ../input/baseline_through2014.csv ../input/baseline_through2022.csv ../input/comparison_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $(START_YEAR) $(CUTOFF_YEARS) $(MINIMUM_MONTHS) "$(STAGE1_CONTROLS)" "$(STAGE1_FE)" $(RAW_MEASURES)

../input/permits_for_uncertainty_index.csv: ../../../../data_raw/score_robustness/permits_for_uncertainty_index.csv | ../input
	ln -sf $< $@

../input/baseline_through2014.csv: ../../score_pair_reliability/output/baseline_through2014.csv | ../input
	ln -sf $< $@

../input/baseline_through2022.csv: ../../score_pair_reliability/output/baseline_through2022.csv | ../input
	ln -sf $< $@

../input/comparison_pairs.csv: ../../score_pair_reliability/output/comparison_pairs.csv | ../input
	ln -sf $< $@

../input/multifamily_score_summary.csv: ../../multifamily_score_uncertainty/output/multifamily_score_summary.csv | ../input
	ln -sf $< $@

../input/multifamily_pair_sensitivity.csv: ../../multifamily_score_uncertainty/output/multifamily_pair_sensitivity.csv | ../input
	ln -sf $< $@

../input/permit_score_summary.csv: ../../permit_score_uncertainty/output/permit_score_summary.csv | ../input
	ln -sf $< $@

../input/permit_pair_sensitivity.csv: ../../permit_score_uncertainty/output/permit_pair_sensitivity.csv | ../input
	ln -sf $< $@

../input/price_score_summary.csv: ../../price_score_uncertainty/output/price_score_summary.csv | ../input
	ln -sf $< $@

../input/sales_pair_sensitivity.csv: ../../price_score_uncertainty/output/sales_pair_sensitivity.csv | ../input
	ln -sf $< $@

../input/rent_pair_sensitivity.csv: ../../price_score_uncertainty/output/rent_pair_sensitivity.csv | ../input
	ln -sf $< $@

link-inputs: ../input/permits_for_uncertainty_index.csv ../input/baseline_through2014.csv ../input/baseline_through2022.csv ../input/comparison_pairs.csv ../input/multifamily_score_summary.csv ../input/multifamily_pair_sensitivity.csv ../input/permit_score_summary.csv ../input/permit_pair_sensitivity.csv ../input/price_score_summary.csv ../input/sales_pair_sensitivity.csv ../input/rent_pair_sensitivity.csv

include ../../../shared/code/generic.make
