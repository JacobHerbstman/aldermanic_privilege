all: ../output/boundary_score_robustness.csv \
	../output/score_stability_summary.csv \
	../output/score_change_decomposition.csv \
	../output/score_period_scale.csv \
	../output/boundary_classification_stability.csv \
	../output/leaveout_classification_stability.csv \
	../output/current_vintage_correlations.csv

../output/score_leaveout_data.rds: \
	build_leaveout_scores.R \
	../../../_lib/alderman_uncertainty_helpers.R \
	../input/permits_for_uncertainty_index.csv \
	../input/alderman_uncertainty_index_through2014.csv \
	../input/alderman_uncertainty_index_through2022.csv \
	../input/new_construction_analysis_data.csv \
	../adjudication/final_project_permit_links.csv \
	../input/rental_rd_characteristics_panel_bw1500.parquet \
	../input/sales_with_hedonics_amenities.parquet | ../output
	$(R) $< 8

../output/score_stability_summary.csv: ../output/score_leaveout_data.rds
	@test -s "$@"

../output/score_change_decomposition.csv: \
	explain_score_changes.R \
	../../../_lib/alderman_uncertainty_helpers.R \
	../input/permits_for_uncertainty_index.csv | ../output
	$(R) $<

../output/score_period_scale.csv: ../output/score_change_decomposition.csv
	@test -s "$@"

../output/boundary_score_robustness.csv: \
	estimate_boundary_checks.R \
	../../../_lib/canonical_geometry_helpers.R \
	../output/score_leaveout_data.rds \
	../input/alderman_uncertainty_index_through2014.csv \
	../input/alderman_uncertainty_index_through2022.csv \
	../input/new_construction_analysis_data.csv \
	../input/rental_rd_characteristics_panel_bw1500.parquet \
	../input/sales_with_hedonics_amenities.parquet | ../output
	$(R) $<

../output/boundary_classification_stability.csv: ../output/boundary_score_robustness.csv
	@test -s "$@"

../output/leaveout_classification_stability.csv: ../output/boundary_score_robustness.csv
	@test -s "$@"

../output/current_vintage_correlations.csv: ../output/boundary_score_robustness.csv
	@test -s "$@"

../input/permits_for_uncertainty_index.csv: ../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2014.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rental_rd_characteristics_panel_bw1500.parquet: ../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw1500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv \
	../input/alderman_uncertainty_index_through2014.csv \
	../input/alderman_uncertainty_index_through2022.csv \
	../input/new_construction_analysis_data.csv \
	../input/rental_rd_characteristics_panel_bw1500.parquet \
	../input/sales_with_hedonics_amenities.parquet

include ../../../shell_functions.make
include ../../../generic.make
