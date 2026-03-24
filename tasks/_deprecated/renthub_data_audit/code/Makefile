SHELL := bash
include ../../shell_functions.make

all: \
	../output/monthly_identifier_coverage.csv \
	../output/monthly_completeness_outages.csv \
	../output/native_unit_repeat_frequency.csv \
	../output/building_layer_repeat_frequency.csv \
	../output/missing_id_fingerprint_frequency.csv \
	../output/same_day_multi_rent_conflicts.csv \
	../output/rent_change_flags.csv \
	../output/repost_reset_flags.csv \
	../output/geo_address_flags.csv \
	../output/company_building_type_composition.csv \
	../output/monthly_rent_series.csv \
	../output/benchmark_fetch_status.csv \
	../output/external_benchmark_comparison.csv \
	../output/monthly_verdict_scorecard.csv \
	../output/subperiod_verdict_scorecard.csv \
	../output/full_panel_verdict.csv \
	../output/recommended_usable_window.csv \
	../output/manual_review_top_cases.csv \
	../output/internal_test_results.csv \
	../output/fig_monthly_verdict_heatmap.pdf \
	../output/fig_scrape_day_completeness_timeline.pdf \
	../output/fig_unit_id_coverage_timeline.pdf \
	../output/fig_duplicate_frequency_timeline.pdf \
	../output/fig_rent_volatility_timeline.pdf \
	../output/fig_normalized_rent_overlay.pdf \
	../output/fig_monthly_yoy_comparison.pdf

../output/monthly_verdict_scorecard.csv: run_renthub_data_audit.R ../input/renthub_raw | ../output ../temp
	$(R) run_renthub_data_audit.R ../input/renthub_raw ../output ../temp

../output/monthly_identifier_coverage.csv \
../output/monthly_completeness_outages.csv \
../output/native_unit_repeat_frequency.csv \
../output/building_layer_repeat_frequency.csv \
../output/missing_id_fingerprint_frequency.csv \
../output/same_day_multi_rent_conflicts.csv \
../output/rent_change_flags.csv \
../output/repost_reset_flags.csv \
../output/geo_address_flags.csv \
../output/company_building_type_composition.csv \
../output/monthly_rent_series.csv \
../output/benchmark_fetch_status.csv \
../output/external_benchmark_comparison.csv \
../output/subperiod_verdict_scorecard.csv \
../output/full_panel_verdict.csv \
../output/recommended_usable_window.csv \
../output/manual_review_top_cases.csv \
../output/internal_test_results.csv \
../output/fig_monthly_verdict_heatmap.pdf \
../output/fig_scrape_day_completeness_timeline.pdf \
../output/fig_unit_id_coverage_timeline.pdf \
../output/fig_duplicate_frequency_timeline.pdf \
../output/fig_rent_volatility_timeline.pdf \
../output/fig_normalized_rent_overlay.pdf \
../output/fig_monthly_yoy_comparison.pdf: ../output/monthly_verdict_scorecard.csv
	test -f $@

../input/renthub_raw: ../../download_rent_data/output | ../input
	ln -sfn $< $@

link-inputs: ../input/renthub_raw

include ../../generic.make
