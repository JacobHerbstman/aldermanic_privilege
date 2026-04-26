SHELL := bash
include ../../shell_functions.make

all: \
	../output/rental_listing_panel_2023_guangbin.parquet \
	../output/rental_listing_panel_2023_guangbin_summary.csv \
	../output/rental_listing_panel_2023_guangbin_trim_diagnostic.csv \
	../output/event_study_coefficients_guangbin_quarterly_mf.csv \
	../output/event_study_support_guangbin_quarterly_mf.csv \
	../output/event_study_pretrend_guangbin_quarterly_mf.csv \
	../output/event_study_metadata_guangbin_quarterly_mf.csv \
	../output/event_study_summary_guangbin_quarterly_mf.csv \
	../output/event_study_guangbin_quarterly_mf.pdf \
	../output/event_study_coefficients_guangbin_quarterly_full.csv \
	../output/event_study_support_guangbin_quarterly_full.csv \
	../output/event_study_pretrend_guangbin_quarterly_full.csv \
	../output/event_study_metadata_guangbin_quarterly_full.csv \
	../output/event_study_summary_guangbin_quarterly_full.csv \
	../output/event_study_guangbin_quarterly_full.pdf \
	../output/event_study_summary_guangbin_quarterly_comparison.csv

../output/rental_listing_panel_2023_guangbin_summary.csv: build_guangbin_panel.R ../input/rental_listing_panel_2023.parquet ../input/renthub_raw ../input/renthub_trim_sensitivity_summary.csv | ../output ../temp
	$(R) build_guangbin_panel.R ../input/rental_listing_panel_2023.parquet ../input/renthub_raw ../input/renthub_trim_sensitivity_summary.csv ../output/rental_listing_panel_2023_guangbin.parquet ../output/rental_listing_panel_2023_guangbin_summary.csv ../output/rental_listing_panel_2023_guangbin_trim_diagnostic.csv

../output/rental_listing_panel_2023_guangbin.parquet: ../output/rental_listing_panel_2023_guangbin_summary.csv
	test -f "$@"

../output/rental_listing_panel_2023_guangbin_trim_diagnostic.csv: ../output/rental_listing_panel_2023_guangbin_summary.csv
	test -f "$@"

../output/event_study_summary_guangbin_quarterly_mf.csv: run_guangbin_event_study.R ../output/rental_listing_panel_2023_guangbin.parquet | ../output
	$(R) run_guangbin_event_study.R ../output/rental_listing_panel_2023_guangbin.parquet multifamily_only ../output/event_study_coefficients_guangbin_quarterly_mf.csv ../output/event_study_support_guangbin_quarterly_mf.csv ../output/event_study_pretrend_guangbin_quarterly_mf.csv ../output/event_study_metadata_guangbin_quarterly_mf.csv ../output/event_study_summary_guangbin_quarterly_mf.csv ../output/event_study_guangbin_quarterly_mf.pdf

../output/event_study_coefficients_guangbin_quarterly_mf.csv: ../output/event_study_summary_guangbin_quarterly_mf.csv
	test -f "$@"

../output/event_study_support_guangbin_quarterly_mf.csv: ../output/event_study_summary_guangbin_quarterly_mf.csv
	test -f "$@"

../output/event_study_pretrend_guangbin_quarterly_mf.csv: ../output/event_study_summary_guangbin_quarterly_mf.csv
	test -f "$@"

../output/event_study_metadata_guangbin_quarterly_mf.csv: ../output/event_study_summary_guangbin_quarterly_mf.csv
	test -f "$@"

../output/event_study_guangbin_quarterly_mf.pdf: ../output/event_study_summary_guangbin_quarterly_mf.csv
	test -f "$@"

../output/event_study_summary_guangbin_quarterly_full.csv: run_guangbin_event_study.R ../output/rental_listing_panel_2023_guangbin.parquet | ../output
	$(R) run_guangbin_event_study.R ../output/rental_listing_panel_2023_guangbin.parquet full_sample ../output/event_study_coefficients_guangbin_quarterly_full.csv ../output/event_study_support_guangbin_quarterly_full.csv ../output/event_study_pretrend_guangbin_quarterly_full.csv ../output/event_study_metadata_guangbin_quarterly_full.csv ../output/event_study_summary_guangbin_quarterly_full.csv ../output/event_study_guangbin_quarterly_full.pdf

../output/event_study_coefficients_guangbin_quarterly_full.csv: ../output/event_study_summary_guangbin_quarterly_full.csv
	test -f "$@"

../output/event_study_support_guangbin_quarterly_full.csv: ../output/event_study_summary_guangbin_quarterly_full.csv
	test -f "$@"

../output/event_study_pretrend_guangbin_quarterly_full.csv: ../output/event_study_summary_guangbin_quarterly_full.csv
	test -f "$@"

../output/event_study_metadata_guangbin_quarterly_full.csv: ../output/event_study_summary_guangbin_quarterly_full.csv
	test -f "$@"

../output/event_study_guangbin_quarterly_full.pdf: ../output/event_study_summary_guangbin_quarterly_full.csv
	test -f "$@"

../output/event_study_summary_guangbin_quarterly_comparison.csv: summarize_guangbin_results.R ../output/event_study_summary_guangbin_quarterly_mf.csv ../output/event_study_summary_guangbin_quarterly_full.csv | ../output
	$(R) summarize_guangbin_results.R ../output/event_study_summary_guangbin_quarterly_mf.csv ../output/event_study_summary_guangbin_quarterly_full.csv ../output/event_study_summary_guangbin_quarterly_comparison.csv

../input/rental_listing_panel_2023.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2023.parquet | ../input
	ln -sf "$<" "$@"

../input/renthub_raw: ../../download_rent_data/output | ../input
	ln -sfn "$<" "$@"

../input/renthub_trim_sensitivity_summary.csv: ../../compare_renthub_dwellsy_aggregate/output/renthub_trim_sensitivity_summary.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rental_listing_panel_2023.parquet ../input/renthub_raw ../input/renthub_trim_sensitivity_summary.csv

include ../../generic.make
