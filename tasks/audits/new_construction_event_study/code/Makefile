all: ../output/new_construction_event_studies.pdf ../output/new_construction_count_specification_comparison.pdf ../output/new_construction_continuous_event_studies.pdf ../output/new_construction_continuous_count_specification_comparison.pdf ../output/new_construction_event_study_pooled.csv ../output/new_construction_event_study_support.csv ../output/new_construction_event_study_pretrends.csv ../output/new_construction_project_coverage.csv

../output/new_construction_block_year_panel.parquet: build_new_construction_event_panel.R ../input/new_construction_analysis_data.csv ../input/final_new_construction_audit_ledger.csv ../input/census_blocks_2010.csv ../input/permit_block_year_panel_2015.parquet ../input/current_income_scores.csv | ../output
	$(R) $<

../output/new_construction_project_block_assignments.csv \
../output/new_construction_project_coverage.csv: ../output/new_construction_block_year_panel.parquet
	@test -s "$@"

../output/new_construction_event_studies.pdf: run_new_construction_event_studies.R ../output/new_construction_block_year_panel.parquet | ../output
	$(R) $<

../output/new_construction_count_event_study.pdf \
../output/new_construction_count_permit_controls_event_study.pdf \
../output/new_construction_count_no_controls_event_study.pdf \
../output/new_construction_count_specification_comparison.pdf \
../output/new_construction_continuous_count_event_study.pdf \
../output/new_construction_continuous_count_permit_controls_event_study.pdf \
../output/new_construction_continuous_count_no_controls_event_study.pdf \
../output/new_construction_continuous_count_specification_comparison.pdf \
../output/new_construction_continuous_far_event_study.pdf \
../output/new_construction_continuous_dupac_event_study.pdf \
../output/new_construction_continuous_event_studies.pdf \
../output/new_construction_far_event_study.pdf \
../output/new_construction_dupac_event_study.pdf \
../output/new_construction_event_study_coefficients.csv \
../output/new_construction_event_study_pooled.csv \
../output/new_construction_event_study_support.csv \
../output/new_construction_event_study_pretrends.csv: ../output/new_construction_event_studies.pdf
	@test -s "$@"

../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/final_new_construction_audit_ledger.csv: ../../commercial_new_construction_sample_audit/output/final_new_construction_audit_ledger.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/census_blocks_2010.csv: ../../../download_chicago_spatial_data/output/census_blocks_2010.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/current_income_scores.csv: ../../income_score_current_results/output/current_income_scores.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/new_construction_analysis_data.csv ../input/final_new_construction_audit_ledger.csv ../input/census_blocks_2010.csv ../input/permit_block_year_panel_2015.parquet ../input/current_income_scores.csv

include ../../../shell_functions.make
include ../../../generic.make
