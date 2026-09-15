include ../../../shared/code/shell_functions.make

START_YEAR := 2006
END_YEAR := 2022
BANDWIDTH_FT := 500
BIN_WIDTH_FT := 100
CONTROLS := share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own
FIXED_EFFECTS := zone_group + segment_id + construction_year
CLUSTER := ward_pair
WORKERS := 4

all: ../output/density_drivers.html

../output/density_drivers.html ../output/density_driver_summary.csv ../output/leading_pair_assessor_history.csv ../output/density_drivers.pdf ../output/density_drivers.png: render_density_drivers.R Makefile ../output/all_driver_results.csv ../output/all_project_evidence.csv ../output/all_pair_sides.csv ../output/multifamily_driver_results.csv ../output/multifamily_project_evidence.csv ../output/multifamily_pair_sides.csv ../input/raw_processing_pairs.csv ../input/commercial_valuation_data.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $<

../output/%_driver_results.csv ../output/%_project_evidence.csv ../output/%_pair_sides.csv: estimate_density_drivers.R Makefile ../input/new_construction_analysis_data.csv ../input/raw_processing_pairs.csv ../input/density_%_results.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $* $(START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) "$(CONTROLS)" "$(FIXED_EFFECTS)" $(CLUSTER) $(WORKERS)

../input/new_construction_analysis_data.csv: ../../../../data_raw/score_robustness/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/raw_processing_pairs.csv: ../../score_robustness_review/output/raw_processing_pairs.csv | ../input
	ln -sf $< $@

../input/commercial_valuation_data.csv: ../../../../data_raw/construction_review/commercial_valuation_data.csv | ../input
	ln -sf $< $@

../input/density_%_results.csv: ../../raw_log_score_sensitivity/records/density_%_before_corrections.csv | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv ../input/raw_processing_pairs.csv ../input/commercial_valuation_data.csv ../input/density_all_results.csv ../input/density_multifamily_results.csv

include ../../../shared/code/generic.make
