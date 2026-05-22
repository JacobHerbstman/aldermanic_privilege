SHELL := bash

.PHONY: all link-inputs

all: ../output/multifamily_data_cleaned.csv ../output/commercial_multi_pin_land_audit.csv ../output/commercial_multi_pin_land_audit_summary.csv ../output/commercial_land_corrections.csv ../output/commercial_density_review_flags.csv ../output/commercial_selected_vs_latest_audit.csv ../output/commercial_unit_definition_audit.csv ../output/commercial_questionable_apartment_audit.csv

../output/multifamily_data_cleaned.csv: commercial_value_data_cleaning.R ../input/commercial_value_raw.csv | ../output
	$(R) $<

../output/commercial_multi_pin_land_audit.csv: ../output/multifamily_data_cleaned.csv
	@test -s "$@"

../output/commercial_multi_pin_land_audit_summary.csv: ../output/multifamily_data_cleaned.csv
	@test -s "$@"

../output/commercial_land_corrections.csv: ../output/multifamily_data_cleaned.csv
	@test -s "$@"

../output/commercial_density_review_flags.csv: ../output/multifamily_data_cleaned.csv
	@test -s "$@"

../output/commercial_selected_vs_latest_audit.csv: ../output/multifamily_data_cleaned.csv
	@test -s "$@"

../output/commercial_unit_definition_audit.csv: ../output/multifamily_data_cleaned.csv
	@test -s "$@"

../output/commercial_questionable_apartment_audit.csv: ../output/multifamily_data_cleaned.csv
	@test -s "$@"

../input/commercial_value_raw.csv: ../../download_commercial_valuation_data/output/commercial_valuation_data.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/commercial_value_raw.csv

include ../../shell_functions.make
include ../../generic.make
