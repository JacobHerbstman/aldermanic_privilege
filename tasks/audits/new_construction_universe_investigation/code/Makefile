SHELL := bash

all: ../output/new_construction_universe_investigation_report.md

../output/new_construction_universe_investigation_report.md: run_new_construction_universe_investigation.R ../input/residential_improvement_characteristics.csv ../input/residential_cross_section.csv ../input/commercial_value_raw.csv ../input/multifamily_data_cleaned.csv ../input/census_bps_chicago_benchmark.csv ../input/outcome_vs_bps_yearly_gap.csv | ../output
	$(R) run_new_construction_universe_investigation.R

../input/residential_improvement_characteristics.csv: ../../../../data_raw/Assessor_-_Single_and_Multi-Family_Improvement_Characteristics_20251111.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/residential_cross_section.csv: ../../../residential_improvements_data_cleaning/output/residential_cross_section.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/commercial_value_raw.csv: ../../../../data_raw/Assessor_-_Commercial_Valuation_Data_20251120.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/multifamily_data_cleaned.csv: ../../../commercial_value_data_cleaning/output/multifamily_data_cleaned.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/census_bps_chicago_benchmark.csv: ../../../audits/new_construction_completeness_audit/output/census_bps_chicago_benchmark.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/outcome_vs_bps_yearly_gap.csv: ../../../audits/new_construction_completeness_audit/output/outcome_vs_bps_yearly_gap.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/residential_improvement_characteristics.csv ../input/residential_cross_section.csv ../input/commercial_value_raw.csv ../input/multifamily_data_cleaned.csv ../input/census_bps_chicago_benchmark.csv ../input/outcome_vs_bps_yearly_gap.csv

include ../../../shell_functions.make
include ../../../generic.make
