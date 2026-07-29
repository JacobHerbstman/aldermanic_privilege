all: ../output/audit_summary.md

../output/audit_summary.md: audit_rental_data_cleaning.R ../input/renthub_raw ../input/chicago_rent_panel.parquet ../input/chicago_rent_panel_quality_flags.parquet ../input/rent_with_ward_distances.parquet ../input/rental_rd_characteristics_panel_bw500.parquet | ../output ../temp
	$(R) $<

../output/cleaning_stage_counts.csv \
../output/raw_city_coverage.csv \
../output/monthly_coverage.csv \
../output/unit_id_contract_summary.csv \
../output/unit_id_collision_examples.csv \
../output/panel_identity_summary.csv \
../output/floorplan_proxy_duplicate_summary.csv \
../output/floorplan_proxy_duplicate_examples.csv \
../output/final_sample_attrition.csv \
../output/final_sample_quality_balance.csv \
../output/final_sample_flag_prevalence.csv \
../output/sample_selection_balance.csv \
../output/building_type_conflict_summary.csv \
../output/manual_address_influence.csv \
../output/model_sensitivity.csv: ../output/audit_summary.md
	@test -s "$@"

../input/renthub_raw: ../../../download_rent_data/output | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sfn "$<" "$@"

../input/chicago_rent_panel.parquet: ../../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_rent_panel_quality_flags.parquet: ../../renthub_quality_diagnostics/output/chicago_rent_panel_quality_flags.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rent_with_ward_distances.parquet: ../../../merge_rent_scores/output/rent_with_ward_distances_full_through2022.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rental_rd_characteristics_panel_bw500.parquet: ../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/renthub_raw ../input/chicago_rent_panel.parquet ../input/chicago_rent_panel_quality_flags.parquet ../input/rent_with_ward_distances.parquet ../input/rental_rd_characteristics_panel_bw500.parquet

include ../../../shell_functions.make
include ../../../generic.make
