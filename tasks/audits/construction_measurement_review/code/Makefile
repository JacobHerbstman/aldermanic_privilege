include ../../../shared/code/shell_functions.make

FIRST_YEAR := 2006
LAST_YEAR := 2022
MAIN_WINDOW_FT := 500
NEARBY_FT := 1000
YEAR_GAP := 2
REPEATED_UNITS_MIN := 8
REPEATED_LAND_MIN := 20000
SMALL_SQFT_PER_UNIT := 300
LARGE_SQFT_PER_UNIT := 5000

all: ../output/measurement_review.html ../output/flag_reconciliation.csv

../output/measurement_review.html ../output/flag_reconciliation.csv: render_measurement_review.R Makefile ../output/project_screen.csv ../output/measurement_pairs.csv ../output/commercial_source_rows.csv ../output/flagged_permits.csv ../output/source_rule_checks.csv ../output/source_rule_summary.csv ../input/prior_unit_review.csv ../review_notes.csv ../prior_decision_review.csv ../count_land_review.csv ../influential_building_review.csv ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $<

../output/source_rule_checks.csv ../output/source_rule_summary.csv: check_source_rules.R Makefile ../output/project_screen.csv ../input/assessor_buildings.csv ../input/measurement_history.csv ../input/residential_history.parquet ../input/condominium_history.csv ../input/recorded_changes.csv ../input/commercial_valuation_data.csv ../input/building_permits_full.csv ../../../shared/code/assessor_classification.R ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $<

../output/project_screen.csv ../output/measurement_pairs.csv ../output/commercial_source_rows.csv ../output/flagged_permits.csv: screen_measurements.R Makefile ../input/project_ledger.csv ../input/assessor_buildings.csv ../input/measurement_history.csv ../input/boundary_scope.csv ../input/analysis_data.csv ../input/prior_unit_review.csv ../input/commercial_valuation_data.csv ../input/building_permit_evidence.csv ../input/building_permits_full.csv ../input/parcel_addresses.csv ../../../shared/code/assessor_classification.R ../../../shared/code/save_data.R ../../../shared/code/report.py | ../output ../report
	$(R) $< $(FIRST_YEAR) $(LAST_YEAR) $(MAIN_WINDOW_FT) $(NEARBY_FT) $(YEAR_GAP) $(REPEATED_UNITS_MIN) $(REPEATED_LAND_MIN) $(SMALL_SQFT_PER_UNIT) $(LARGE_SQFT_PER_UNIT)

../input/project_ledger.csv: ../records/project_ledger_before_adoption.csv | ../input
	ln -sf $< $@
../input/assessor_buildings.csv: ../../../prepare_new_construction/output/assessor_buildings.csv | ../input
	ln -sf $< $@
../input/measurement_history.csv: ../../../prepare_new_construction/output/assessor_measurement_records.csv | ../input
	ln -sf $< $@
../input/boundary_scope.csv: ../records/boundary_scope_before_adoption.csv | ../input
	ln -sf $< $@
../input/analysis_data.csv: ../../../../data_raw/score_robustness/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/prior_unit_review.csv: ../../construction_review_history/records/commercial_unit_adjudication_evidence.csv | ../input
	ln -sf $< $@
../input/commercial_valuation_data.csv: ../../../../data_raw/construction_review/commercial_valuation_data.csv | ../input
	ln -sf $< $@
../input/building_permit_evidence.csv: ../../../prepare_new_construction/output/building_permit_evidence.csv | ../input
	ln -sf $< $@
../input/building_permits_full.csv: ../../../../data_raw/construction_review/building_permits_full.csv | ../input
	ln -sf $< $@
../input/parcel_addresses.csv: ../../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	ln -sf $< $@
../input/condominium_history.csv: ../../../download_construction_assessor_history/output/construction_condominium_history.csv | ../input
	ln -sf $< $@
../input/recorded_changes.csv: ../records/recorded_changes_before_adoption.csv | ../input
	ln -sf $< $@
../input/residential_history.parquet: ../../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/project_ledger.csv ../input/assessor_buildings.csv ../input/measurement_history.csv ../input/boundary_scope.csv ../input/analysis_data.csv ../input/prior_unit_review.csv ../input/commercial_valuation_data.csv ../input/building_permit_evidence.csv ../input/building_permits_full.csv ../input/parcel_addresses.csv ../input/condominium_history.csv ../input/recorded_changes.csv ../input/residential_history.parquet

include ../../../shared/code/generic.make
