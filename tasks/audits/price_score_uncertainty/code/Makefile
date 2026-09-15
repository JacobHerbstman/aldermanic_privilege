include ../../../shared/code/shell_functions.make

SALES_START_YEAR := 2006
RENT_START_YEAR := 2014
END_YEAR := 2022
BANDWIDTH_FT := 500
BIN_WIDTH_FT := 100
SALES_CONTROLS := log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor
RENT_CONTROLS := log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft + building_type_factor
SALES_FIXED_EFFECTS := segment_id^year_quarter
RENT_FIXED_EFFECTS := segment_id^year_month
CLUSTER := ward_pair
FRAGILE_THRESHOLDS := 0.75,0.95
WORKERS := 4

all: ../output/price_score_sensitivity.html

../output/price_score_sensitivity.html ../output/price_score_summary.csv ../output/price_pair_influence.csv ../output/price_score_draws.pdf ../output/price_score_draws.png: render_price_sensitivity.R Makefile ../output/sales_score_draws.csv ../output/sales_pair_sensitivity.csv ../output/sales_pairs.csv ../output/rent_score_draws.csv ../output/rent_pair_sensitivity.csv ../output/rent_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $<

../output/sales_score_draws.csv ../output/sales_pair_sensitivity.csv ../output/sales_pairs.csv: estimate_price_sensitivity.R Makefile ../input/sales_with_hedonics_amenities.parquet ../input/baseline_through2022.csv ../input/resamples_through2022.parquet ../input/bootstrap_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< sales $(SALES_START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) "$(SALES_CONTROLS)" "$(SALES_FIXED_EFFECTS)" $(CLUSTER) $(FRAGILE_THRESHOLDS) $(WORKERS)

../output/rent_score_draws.csv ../output/rent_pair_sensitivity.csv ../output/rent_pairs.csv: estimate_price_sensitivity.R Makefile ../input/rental_rd_characteristics_panel_bw1500.parquet ../input/baseline_through2022.csv ../input/resamples_through2022.parquet ../input/bootstrap_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< rent $(RENT_START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) "$(RENT_CONTROLS)" "$(RENT_FIXED_EFFECTS)" $(CLUSTER) $(FRAGILE_THRESHOLDS) $(WORKERS)

../input/sales_with_hedonics_amenities.parquet: ../../../../data_raw/score_robustness/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@

../input/rental_rd_characteristics_panel_bw1500.parquet: ../../../../data_raw/score_robustness/rental_rd_characteristics_panel_bw1500.parquet | ../input
	ln -sf $< $@

../input/baseline_through2022.csv: ../../score_pair_reliability/output/baseline_through2022.csv | ../input
	ln -sf $< $@

../input/resamples_through2022.parquet: ../../score_pair_reliability/output/resamples_through2022.parquet | ../input
	ln -sf $< $@

../input/bootstrap_pairs.csv: ../../score_pair_reliability/output/bootstrap_pairs.csv | ../input
	ln -sf $< $@

link-inputs: ../input/sales_with_hedonics_amenities.parquet ../input/rental_rd_characteristics_panel_bw1500.parquet ../input/baseline_through2022.csv ../input/resamples_through2022.parquet ../input/bootstrap_pairs.csv

include ../../../shared/code/generic.make
