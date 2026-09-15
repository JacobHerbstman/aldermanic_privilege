include ../../../shared/code/shell_functions.make

START_YEAR := 2006
RENT_START_YEAR := 2014
END_YEAR := 2022
BANDWIDTH_FT := 500
BIN_WIDTH_FT := 100
DENSITY_CONTROLS := share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own
DENSITY_FE := zone_group + segment_id + construction_year
SALES_CONTROLS := log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor
SALES_FE := segment_id^year_quarter
RENT_CONTROLS := log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft + building_type_factor
RENT_FE := segment_id^year_month
BOUNDARY_CLUSTER := ward_pair
PERMIT_OUTCOME := n_high_discretion_application
REMAP_YEAR := 2015
EVENT_WINDOW := 5
PERMIT_FE := block_id + ward_pair_id^year
PERMIT_CLUSTER := ward_pair_id

all: ../output/raw_log_results.html

../output/raw_log_results.html ../output/raw_log_summary.csv ../output/raw_log_effects.pdf ../output/raw_log_effects.png ../output/raw_log_permit_events.pdf ../output/raw_log_permit_events.png: render_raw_log_results.R Makefile ../output/density_all_results.csv ../output/density_multifamily_results.csv ../output/sales_results.csv ../output/rent_results.csv ../output/permit_results.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $<

../output/density_%_results.csv: estimate_boundary_results.R Makefile ../input/new_construction_analysis_data.csv ../input/raw_processing_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< density_$* $(START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) "$(DENSITY_CONTROLS)" "$(DENSITY_FE)" $(BOUNDARY_CLUSTER)

../output/sales_results.csv: estimate_boundary_results.R Makefile ../input/sales_with_hedonics_amenities.parquet ../input/raw_processing_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< sales $(START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) "$(SALES_CONTROLS)" "$(SALES_FE)" $(BOUNDARY_CLUSTER)

../output/rent_results.csv: estimate_boundary_results.R Makefile ../input/rental_rd_characteristics_panel_bw1500.parquet ../input/raw_processing_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< rent $(RENT_START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) "$(RENT_CONTROLS)" "$(RENT_FE)" $(BOUNDARY_CLUSTER)

../output/permit_results.csv: estimate_permit_results.R Makefile ../input/permit_block_year_panel_2015.parquet ../input/raw_processing_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $(PERMIT_OUTCOME) $(REMAP_YEAR) $(EVENT_WINDOW) $(BANDWIDTH_FT) "$(PERMIT_FE)" $(PERMIT_CLUSTER)

../input/raw_processing_pairs.csv: ../../score_robustness_review/output/raw_processing_pairs.csv | ../input
	ln -sf $< $@

../input/new_construction_analysis_data.csv: ../../../../data_raw/score_robustness/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/sales_with_hedonics_amenities.parquet: ../../../../data_raw/score_robustness/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@

../input/rental_rd_characteristics_panel_bw1500.parquet: ../../../../data_raw/score_robustness/rental_rd_characteristics_panel_bw1500.parquet | ../input
	ln -sf $< $@

../input/permit_block_year_panel_2015.parquet: ../../../../data_raw/score_robustness/permit_block_year_panel_2015.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/raw_processing_pairs.csv ../input/new_construction_analysis_data.csv ../input/sales_with_hedonics_amenities.parquet ../input/rental_rd_characteristics_panel_bw1500.parquet ../input/permit_block_year_panel_2015.parquet

include ../../../shared/code/generic.make
