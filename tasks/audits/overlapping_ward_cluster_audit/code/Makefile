all: ../output/cluster_inference_comparison.csv ../output/ward_pair_network_summary.csv ../output/leave_one_ward_out_estimates.csv ../output/leave_one_ward_out_summary.csv

../output/cluster_inference_comparison.csv: audit_overlapping_ward_clusters.R ../input/parcels_with_ward_distances.csv ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $<

../output/ward_pair_network_summary.csv \
../output/leave_one_ward_out_estimates.csv \
../output/leave_one_ward_out_summary.csv: ../output/cluster_inference_comparison.csv
	@test -s "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rental_rd_characteristics_panel_bw500.parquet: ../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permit_block_year_panel_2015.parquet

include ../../../shell_functions.make
include ../../../generic.make
