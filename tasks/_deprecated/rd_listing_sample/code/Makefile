all: \
	../output/join_coverage_summary.csv \
	../output/listing_key_source_by_month.csv \
	../output/day_drop_reason_by_month.csv \
	../output/month_drop_reason_by_month.csv \
	../output/monthly_rd_quality_overall.csv \
	../output/monthly_rd_quality_by_side.csv \
	../output/monthly_rd_quality_side_gaps.csv \
	../output/pair_month_support.csv \
	../output/candidate_windows.csv \
	../output/window_recommendation.csv \
	../output/manual_review_worst_cases.csv \
	../output/rd_listing_day_strict.parquet \
	../output/rd_listing_month_flagged.parquet \
	../output/rd_listing_month_ultra.parquet \
	../output/fig_monthly_keep_rates.pdf \
	../output/fig_monthly_side_gaps.pdf \
	../output/fig_candidate_window_timeline.pdf

../output/window_recommendation.csv: run_rd_listing_sample.R ../input/renthub_raw ../input/rent_with_ward_distances_full.parquet | ../output ../temp
	Rscript run_rd_listing_sample.R ../input/renthub_raw ../input/rent_with_ward_distances_full.parquet ../output ../temp

../output/join_coverage_summary.csv: ../output/window_recommendation.csv
	test -f "$@"

../output/listing_key_source_by_month.csv: ../output/window_recommendation.csv
	test -f "$@"

../output/day_drop_reason_by_month.csv: ../output/window_recommendation.csv
	test -f "$@"

../output/month_drop_reason_by_month.csv: ../output/window_recommendation.csv
	test -f "$@"

../output/monthly_rd_quality_overall.csv: ../output/window_recommendation.csv
	test -f "$@"

../output/monthly_rd_quality_by_side.csv: ../output/window_recommendation.csv
	test -f "$@"

../output/monthly_rd_quality_side_gaps.csv: ../output/window_recommendation.csv
	test -f "$@"

../output/pair_month_support.csv: ../output/window_recommendation.csv
	test -f "$@"

../output/candidate_windows.csv: ../output/window_recommendation.csv
	test -f "$@"

../output/manual_review_worst_cases.csv: ../output/window_recommendation.csv
	test -f "$@"

../output/rd_listing_day_strict.parquet: ../output/window_recommendation.csv
	test -f "$@"

../output/rd_listing_month_flagged.parquet: ../output/window_recommendation.csv
	test -f "$@"

../output/rd_listing_month_ultra.parquet: ../output/window_recommendation.csv
	test -f "$@"

../output/fig_monthly_keep_rates.pdf: ../output/window_recommendation.csv
	test -f "$@"

../output/fig_monthly_side_gaps.pdf: ../output/window_recommendation.csv
	test -f "$@"

../output/fig_candidate_window_timeline.pdf: ../output/window_recommendation.csv
	test -f "$@"

../input/renthub_raw: ../../download_rent_data/output | ../input
	ln -sfn "$<" "$@"

../input/rent_with_ward_distances_full.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sfn "$<" "$@"

link-inputs: ../input/renthub_raw ../input/rent_with_ward_distances_full.parquet

include ../../generic.make
