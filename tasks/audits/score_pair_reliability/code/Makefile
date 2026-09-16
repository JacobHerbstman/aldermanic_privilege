include ../../../shared/code/shell_functions.make

START_YEAR := 2006
BOOTSTRAP_DRAWS := 499
SPLIT_DRAWS := 50
SEED := 9142026
WORKERS := 4
STAGE1_CONTROLS := median_hh_income_10k + share_black + share_hisp + share_white + homeownership_rate + pop_total_10k + dist_cbd_km + dist_lake_km + n_rail_stations_800m + n_permits_wm_l1
STAGE1_FE := month + permit_type_clean + review_type_clean
STAGE2_CONTROL := n_permits_wm_l1
MINIMUM_MONTHS := 4
BANDWIDTH_FT := 500
BIN_WIDTH_FT := 100
SPLIT_MINIMUM_PERMITS := 30
SPLIT_MINIMUM_MONTHS := 6
DENSITY_CONTROLS := share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own
RENT_CONTROLS := log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft + building_type_factor
SALES_CONTROLS := log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor
DENSITY_FE := zone_group + segment_id + construction_year
RENT_FE := segment_id^year_month
SALES_FE := segment_id^year_quarter
END_YEAR := 2022
RENT_START_YEAR := 2014
REMAP_YEAR := 2015
EVENT_WINDOW := 5

all: ../output/pair_reliability.html

../output/pair_reliability.html ../output/comparison_pairs.csv ../output/fitted_samples.csv ../output/bootstrap_pairs.csv ../output/split_pairs.parquet ../output/split_summary.csv ../output/split_rank_correlations.csv ../output/bootstrap_summary.csv ../output/split_agreement.pdf ../output/split_agreement.png: summarize_pair_reliability.R Makefile ../output/baseline_through2014.csv ../output/resamples_through2014.parquet ../output/splits_through2014.parquet ../output/baseline_through2022.csv ../output/resamples_through2022.parquet ../output/splits_through2022.parquet ../input/new_construction_analysis_data.csv ../input/rental_rd_characteristics_panel_bw1500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permit_block_year_panel_2015.parquet ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $(BANDWIDTH_FT) $(BIN_WIDTH_FT) $(SPLIT_MINIMUM_PERMITS) $(SPLIT_MINIMUM_MONTHS) "$(DENSITY_CONTROLS)" "$(RENT_CONTROLS)" "$(SALES_CONTROLS)" "$(DENSITY_FE)" "$(RENT_FE)" "$(SALES_FE)" $(START_YEAR) $(END_YEAR) $(RENT_START_YEAR) $(REMAP_YEAR) $(EVENT_WINDOW)

../output/baseline_through%.csv ../output/resamples_through%.parquet ../output/splits_through%.parquet: estimate_score_resamples.R Makefile ../input/permits_for_uncertainty_index.csv ../input/paper_scores.csv ../input/early_scores.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $* $(START_YEAR) $(BOOTSTRAP_DRAWS) $(SPLIT_DRAWS) $(SEED) $(WORKERS) "$(STAGE1_CONTROLS)" "$(STAGE1_FE)" $(STAGE2_CONTROL) $(MINIMUM_MONTHS)

../input/permits_for_uncertainty_index.csv: ../../../../data_raw/score_robustness/permits_for_uncertainty_index.csv | ../input
	ln -sf $< $@

../input/paper_scores.csv: ../../donation_score_robustness/sources/paper_scores.csv | ../input
	ln -sf $< $@

../input/early_scores.csv: ../../donation_score_robustness/sources/early_scores.csv | ../input
	ln -sf $< $@

../input/new_construction_analysis_data.csv: ../../../../data_raw/score_robustness/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/rental_rd_characteristics_panel_bw1500.parquet: ../../../../data_raw/score_robustness/rental_rd_characteristics_panel_bw1500.parquet | ../input
	ln -sf $< $@

../input/sales_with_hedonics_amenities.parquet: ../../../../data_raw/score_robustness/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@

../input/permit_block_year_panel_2015.parquet: ../../../../data_raw/score_robustness/permit_block_year_panel_2015.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/permits_for_uncertainty_index.csv ../input/paper_scores.csv ../input/early_scores.csv ../input/new_construction_analysis_data.csv ../input/rental_rd_characteristics_panel_bw1500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permit_block_year_panel_2015.parquet

include ../../../shared/code/generic.make
