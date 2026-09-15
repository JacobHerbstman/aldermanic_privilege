include ../../../shared/code/shell_functions.make

OUTCOME := n_high_discretion_application
REMAP_YEAR := 2015
EVENT_WINDOW := 5
BANDWIDTH_FT := 500
FIXED_EFFECTS := block_id + ward_pair_id^year
CLUSTER := ward_pair_id
FRAGILE_THRESHOLDS := 0.75,0.95
WORKERS := 4

all: ../output/permit_score_sensitivity.html

../output/permit_score_sensitivity.html ../output/permit_score_summary.csv ../output/permit_pair_influence.csv ../output/permit_score_events.pdf ../output/permit_score_events.png ../output/permit_fragile_events.pdf ../output/permit_fragile_events.png: render_permit_sensitivity.R Makefile ../output/permit_score_draws.csv ../output/permit_pair_sensitivity.csv ../output/permit_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $<

../output/permit_score_draws.csv ../output/permit_pair_sensitivity.csv ../output/permit_pairs.csv: estimate_permit_sensitivity.R Makefile ../input/permit_block_year_panel_2015.parquet ../input/baseline_through2014.csv ../input/resamples_through2014.parquet ../input/bootstrap_pairs.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $(OUTCOME) $(REMAP_YEAR) $(EVENT_WINDOW) $(BANDWIDTH_FT) "$(FIXED_EFFECTS)" $(CLUSTER) $(FRAGILE_THRESHOLDS) $(WORKERS)

../input/permit_block_year_panel_2015.parquet: ../../../../data_raw/score_robustness/permit_block_year_panel_2015.parquet | ../input
	ln -sf $< $@

../input/baseline_through2014.csv: ../../score_pair_reliability/output/baseline_through2014.csv | ../input
	ln -sf $< $@

../input/resamples_through2014.parquet: ../../score_pair_reliability/output/resamples_through2014.parquet | ../input
	ln -sf $< $@

../input/bootstrap_pairs.csv: ../../score_pair_reliability/output/bootstrap_pairs.csv | ../input
	ln -sf $< $@

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/baseline_through2014.csv ../input/resamples_through2014.parquet ../input/bootstrap_pairs.csv

include ../../../shared/code/generic.make
