all: ../output/cta_history_estimate_comparison.csv ../output/cta_history_distance_summary.csv ../output/cta_history_permit_count_summary.csv ../output/cta_history_permit_count_by_year.csv ../output/cta_history_score_comparison.csv ../output/cta_history_score_alderman_changes.csv ../output/cta_history_downstream_estimate_comparison.csv

../output/cta_history_estimate_comparison.csv: audit_cta_station_history.R ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet | ../output
	$(R) $<

../output/cta_history_distance_summary.csv: ../output/cta_history_estimate_comparison.csv
	@test -s "$@"

../output/cta_history_permit_count_summary.csv: audit_permit_cta_history.R ../input/permits_for_uncertainty_index.csv ../input/building_permits_clean.gpkg ../input/cta_stops.gpkg | ../output
	$(R) $<

../output/cta_history_permit_count_by_year.csv: ../output/cta_history_permit_count_summary.csv
	@test -s "$@"

../output/cta_history_score_comparison.csv: ../output/cta_history_permit_count_summary.csv
	@test -s "$@"

../output/cta_history_score_alderman_changes.csv: ../output/cta_history_score_comparison.csv
	@test -s "$@"

../output/cta_history_downstream_estimate_comparison.csv: audit_downstream_results.R ../output/cta_history_score_alderman_changes.csv ../input/parcels_with_ward_distances.csv ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $<

../input/rental_rd_characteristics_panel_bw500.parquet: ../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permits_for_uncertainty_index.csv: ../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/cta_stops.gpkg: ../../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permits_for_uncertainty_index.csv ../input/building_permits_clean.gpkg ../input/cta_stops.gpkg ../input/parcels_with_ward_distances.csv ../input/permit_block_year_panel_2015.parquet

include ../../../shell_functions.make
include ../../../generic.make
