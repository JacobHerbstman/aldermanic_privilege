include ../../../shared/code/shell_functions.make

START_YEAR := 2006
END_YEAR := 2022
BANDWIDTH_FT := 500
BIN_WIDTH_FT := 100
CONTROLS := share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own
FIXED_EFFECTS := zone_group + segment_id + construction_year
CLUSTER := ward_pair
FRAGILE_THRESHOLDS := 0.75,0.95
WORKERS := 4

all: ../output/multifamily_score_sensitivity.html

../output/multifamily_score_sensitivity.html ../output/multifamily_score_summary.csv ../output/multifamily_pair_influence.csv ../output/multifamily_score_draws.pdf ../output/multifamily_score_draws.png: render_multifamily_sensitivity.R Makefile ../output/multifamily_score_draws.csv ../output/multifamily_pair_sensitivity.csv ../output/multifamily_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $<

../output/multifamily_score_draws.csv ../output/multifamily_pair_sensitivity.csv ../output/multifamily_pairs.csv: estimate_multifamily_sensitivity.R Makefile ../input/new_construction_analysis_data.csv ../input/baseline_through2022.csv ../input/resamples_through2022.parquet ../input/bootstrap_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $(START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) "$(CONTROLS)" "$(FIXED_EFFECTS)" $(CLUSTER) $(FRAGILE_THRESHOLDS) $(WORKERS)

../input/new_construction_analysis_data.csv: ../../../../data_raw/score_robustness/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/baseline_through2022.csv: ../../score_pair_reliability/output/baseline_through2022.csv | ../input
	ln -sf $< $@

../input/resamples_through2022.parquet: ../../score_pair_reliability/output/resamples_through2022.parquet | ../input
	ln -sf $< $@

../input/bootstrap_pairs.csv: ../../score_pair_reliability/output/bootstrap_pairs.csv | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv ../input/baseline_through2022.csv ../input/resamples_through2022.parquet ../input/bootstrap_pairs.csv

include ../../../shared/code/generic.make
