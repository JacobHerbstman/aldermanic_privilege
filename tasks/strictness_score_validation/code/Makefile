SHELL := bash
include ../../shell_functions.make

all: ../output/ward_month_validation_regressions.tex ../output/ward_month_validation_coefficients.csv ../output/ward_month_validation_coefficients.pdf ../output/ward_month_strictness_validation_panel.csv ../output/ward_month_validation_coverage.csv ../output/ward_month_validation_sample_cuts.csv ../output/ward_month_validation_permit_volume.csv ../output/ward_month_validation_permit_volume_by_score.csv ../output/ward_month_validation_permit_volume_by_score_summary.csv ../output/uncertainty_nocafe_densitypass_validation_metrics.csv ../output/uncertainty_nocafe_densitypass_validation_scorecard.csv ../output/targeted_fe_sweep_detail.csv ../output/targeted_fe_sweep_scorecard.csv

../output/ward_month_validation_regressions.tex ../output/ward_month_validation_coefficients.csv ../output/ward_month_validation_coefficients.pdf ../output/ward_month_strictness_validation_panel.csv ../output/ward_month_validation_coverage.csv ../output/ward_month_validation_sample_cuts.csv ../output/ward_month_validation_permit_volume.csv: strictness_score_validation_ward_month.R ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/ward_monthly_panel_for_alderman_fe.csv | ../output
	$(R) strictness_score_validation_ward_month.R

../output/ward_month_validation_permit_volume_by_score.csv ../output/ward_month_validation_permit_volume_by_score_summary.csv: compare_permit_volume_across_scores.R ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/ward_monthly_panel_for_alderman_fe.csv ../input/uncertainty_scores | ../output
	$(R) compare_permit_volume_across_scores.R

../output/uncertainty_nocafe_densitypass_validation_metrics.csv ../output/uncertainty_nocafe_densitypass_validation_scorecard.csv: scan_uncertainty_densitypass_validation.R ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/ward_monthly_panel_for_alderman_fe.csv ../input/uncertainty_scores ../../create_alderman_uncertainty_index/output/spec_scan_pairxyear_bw500.csv | ../output
	$(R) scan_uncertainty_densitypass_validation.R

../output/targeted_fe_sweep_detail.csv ../output/targeted_fe_sweep_scorecard.csv: scan_targeted_fe_specs.R ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/uncertainty_scores | ../output
	$(R) scan_targeted_fe_specs.R

../input/building_permits_text_features.csv.gz: ../../building_permits_scraping/output/building_permits_text_features.csv.gz | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/alderman_restrictiveness_scores_month_FEs.csv: ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

../input/ward_monthly_panel_for_alderman_fe.csv: ../../data_for_alderman_strictness_scores/output/ward_monthly_panel_for_alderman_fe.csv | ../input
	ln -sf $< $@

../input/uncertainty_scores: ../../create_alderman_uncertainty_index/output | ../input
	ln -sfn $< $@

include ../../generic.make

link-inputs: ../input/building_permits_text_features.csv.gz ../input/chicago_alderman_panel.csv ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/ward_monthly_panel_for_alderman_fe.csv ../input/uncertainty_scores
