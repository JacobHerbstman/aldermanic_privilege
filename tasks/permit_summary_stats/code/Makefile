SHELL := bash
include ../../shell_functions.make

UNCERTAINTY_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH

all: \
	../output/high_discretion_permit_counts.csv \
	../output/high_discretion_permit_counts.tex \
	../output/permit_processing_time_summary_stats.csv \
	../output/permit_processing_time_summary_stats.tex \
	../output/permit_processing_time_skew_diagnostics.csv \
	../output/permit_right_tail_concentration_summary.csv \
	../output/permit_right_tail_by_alderman.csv \
	../output/permit_right_tail_by_ward.csv \
	../output/processing_time_raw_high_discretion.pdf \
	../output/processing_time_raw_new_construction.pdf \
	../output/processing_time_residualized_high_discretion.pdf \
	../output/processing_time_residualized_new_construction.pdf \
	../output/permit_processing_time_days_distribution.pdf \
	../output/permit_right_tail_top_entities_p95.pdf \
	../output/permit_right_tail_rate_vs_stringency_p95.pdf \
	../output/permit_right_tail_excess_vs_stringency_p95.pdf \
	../output/processing_time_distribution_by_alderman.csv \
	../output/processing_time_distribution_by_alderman_sample.csv \
	../output/processing_time_distribution_by_permit_type.csv \
	../output/processing_time_distribution_by_alderman_type.csv \
	../output/processing_time_variation_across_aldermen_by_type.csv \
	../output/permit_processing_time_by_alderman_raw.pdf \
	../output/permit_processing_time_by_alderman_vs_stringency.pdf \
	../output/permit_processing_time_by_alderman_score_correlation.csv \
	../output/permit_stringency_score_translation.csv \
	../output/permit_stringency_score_translation.tex \
	../output/permit_processing_convergence_monthly_cross_alderman.csv \
	../output/permit_processing_convergence_monthly_overall.csv \
	../output/permit_processing_convergence_prepost_tests.csv \
	../output/permit_processing_convergence_july2019.pdf \
	../output/permit_processing_time_high_vs_low_summary.csv \
	../output/permit_processing_time_high_vs_low_summary.tex \
	../output/ward_mean_processing_time_high_vs_low.csv \
	../output/ward_mean_processing_time_high_vs_low_density.pdf \
	../output/permit_processing_time_high_vs_low_alderman_summary.csv \
	../output/permit_processing_time_high_vs_low_alderman_summary.tex \
	../output/alderman_mean_processing_time_high_vs_low.csv \
	../output/alderman_mean_processing_time_high_vs_low_density.pdf \
	../output/permit_processing_time_volume_correlation.csv \
	../output/permit_processing_time_volume_correlation.tex

../output/high_discretion_permit_counts.csv \
../output/high_discretion_permit_counts.tex \
../output/permit_processing_time_summary_stats.csv \
../output/permit_processing_time_summary_stats.tex \
../output/permit_processing_time_skew_diagnostics.csv \
../output/permit_right_tail_concentration_summary.csv \
../output/permit_right_tail_by_alderman.csv \
../output/permit_right_tail_by_ward.csv \
../output/processing_time_raw_high_discretion.pdf \
../output/processing_time_raw_new_construction.pdf \
../output/processing_time_residualized_high_discretion.pdf \
../output/processing_time_residualized_new_construction.pdf \
../output/permit_processing_time_days_distribution.pdf \
../output/permit_right_tail_top_entities_p95.pdf \
../output/permit_right_tail_rate_vs_stringency_p95.pdf \
../output/permit_right_tail_excess_vs_stringency_p95.pdf \
../output/processing_time_distribution_by_alderman.csv \
../output/processing_time_distribution_by_alderman_sample.csv \
../output/processing_time_distribution_by_permit_type.csv \
../output/processing_time_distribution_by_alderman_type.csv \
../output/processing_time_variation_across_aldermen_by_type.csv \
../output/permit_processing_time_by_alderman_raw.pdf \
../output/permit_processing_time_by_alderman_vs_stringency.pdf \
../output/permit_processing_time_by_alderman_score_correlation.csv \
../output/permit_stringency_score_translation.csv \
../output/permit_stringency_score_translation.tex \
../output/permit_processing_convergence_monthly_cross_alderman.csv \
../output/permit_processing_convergence_monthly_overall.csv \
../output/permit_processing_convergence_prepost_tests.csv \
../output/permit_processing_convergence_july2019.pdf: permit_summary_stats.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../output
	Rscript permit_summary_stats.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../output

../output/permit_processing_time_high_vs_low_summary.csv: permit_processing_time_high_vs_low_summary.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv | ../output
	Rscript permit_processing_time_high_vs_low_summary.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../output/permit_processing_time_high_vs_low_summary.csv ../output/permit_processing_time_high_vs_low_summary.tex ../output/ward_mean_processing_time_high_vs_low.csv ../output/ward_mean_processing_time_high_vs_low_density.pdf ../output/permit_processing_time_high_vs_low_alderman_summary.csv ../output/permit_processing_time_high_vs_low_alderman_summary.tex ../output/alderman_mean_processing_time_high_vs_low.csv ../output/alderman_mean_processing_time_high_vs_low_density.pdf ../output/permit_processing_time_volume_correlation.csv ../output/permit_processing_time_volume_correlation.tex

../output/permit_processing_time_high_vs_low_summary.tex: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/ward_mean_processing_time_high_vs_low.csv: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/ward_mean_processing_time_high_vs_low_density.pdf: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/permit_processing_time_high_vs_low_alderman_summary.csv: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/permit_processing_time_high_vs_low_alderman_summary.tex: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/alderman_mean_processing_time_high_vs_low.csv: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/alderman_mean_processing_time_high_vs_low_density.pdf: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/permit_processing_time_volume_correlation.csv: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../output/permit_processing_time_volume_correlation.tex: ../output/permit_processing_time_high_vs_low_summary.csv
	test -f "$@"

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf "$<" "$@"

../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv

include ../../generic.make
