include ../../../shared/code/shell_functions.make

SECTOR := developer_verified
START_YEAR := 2006
END_YEAR := 2022
RENT_START_YEAR := 2014
REMAP_YEAR := 2015
BANDWIDTH_FT := 500
BIN_WIDTH_FT := 100
EVENT_WINDOW := 5
DENSITY_CONTROLS := share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own
DENSITY_FE := zone_group + segment_id + construction_year
RENT_CONTROLS := log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft + building_type_factor
RENT_FE := segment_id^year_month
SALES_CONTROLS := log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor
SALES_FE := segment_id^year_quarter
BOUNDARY_CLUSTER := ward_pair
PERMIT_FE := block_id + ward_pair_id^year
PERMIT_CLUSTER := ward_pair_id

all: ../output/developer_results.html ../output/developer_scores.csv

../output/developer_results.html: render_developer_results.R Makefile ../output/developer_estimates.csv ../output/developer_profiles.csv ../output/developer_coverage.csv ../output/permit_pair_comparisons.csv ../output/permit_ordering_results.csv ../output/permit_reassignment_flows.csv | ../output
	$(R) $<

../output/permit_pair_comparisons.csv ../output/permit_ordering_results.csv ../output/permit_reassignment_flows.csv: compare_permit_pairs.R Makefile ../input/permit_block_year_panel_2015.parquet ../output/permit_comparison_panel.parquet ../output/developer_scores.csv ../output/developer_estimates.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< "$(PERMIT_FE)" $(PERMIT_CLUSTER)

../output/developer_estimates.csv ../output/developer_profiles.csv ../output/developer_coverage.csv ../output/developer_scores.csv ../output/permit_comparison_panel.parquet: estimate_developer_results.R Makefile ../input/donation_receipts.parquet ../input/new_construction_analysis_data.csv ../input/rental_rd_characteristics_panel_bw1500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permit_block_year_panel_2015.parquet ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $(SECTOR) $(START_YEAR) $(END_YEAR) $(RENT_START_YEAR) $(REMAP_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) $(EVENT_WINDOW) "$(DENSITY_CONTROLS)" "$(DENSITY_FE)" "$(RENT_CONTROLS)" "$(RENT_FE)" "$(SALES_CONTROLS)" "$(SALES_FE)" $(BOUNDARY_CLUSTER) "$(PERMIT_FE)" $(PERMIT_CLUSTER)

../input/donation_receipts.parquet: ../../donation_score_robustness/output/donation_receipts.parquet | ../input
	ln -sf $< $@

../input/new_construction_analysis_data.csv: ../../../../data_raw/score_robustness/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/rental_rd_characteristics_panel_bw1500.parquet: ../../../../data_raw/score_robustness/rental_rd_characteristics_panel_bw1500.parquet | ../input
	ln -sf $< $@

../input/sales_with_hedonics_amenities.parquet: ../../../../data_raw/score_robustness/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@

../input/permit_block_year_panel_2015.parquet: ../../../../data_raw/score_robustness/permit_block_year_panel_2015.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/donation_receipts.parquet ../input/new_construction_analysis_data.csv ../input/rental_rd_characteristics_panel_bw1500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permit_block_year_panel_2015.parquet

include ../../../shared/code/generic.make
