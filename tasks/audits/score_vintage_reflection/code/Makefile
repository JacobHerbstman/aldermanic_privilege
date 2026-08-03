all: ../output/boundary_score_robustness.csv \
	../output/score_stability_summary.csv \
	../output/score_change_decomposition.csv \
	../output/score_period_scale.csv \
	../output/regime_score_stability.csv \
	../output/regime_score_comparison.csv \
	../output/regime_period_scale.csv \
	../output/regime_density_results.csv \
	../output/regime_classification_stability.csv \
	../output/regime_flip_contributors.csv \
	../output/regime_score_coverage.csv \
	../output/regime_density_rd_pooled_full.pdf \
	../output/regime_density_rd_pooled_common.pdf \
	../output/regime_density_rd_dated_common.pdf \
	../output/ward_36_geography_change.csv \
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

../output/regime_scores.rds: \
	build_regime_scores.R \
	../../../_lib/alderman_uncertainty_helpers.R \
	../input/permits_for_uncertainty_index.csv \
	../input/alderman_uncertainty_index_through2014.csv | ../output
	$(R) $<

../output/regime_score_stability.csv: ../output/regime_scores.rds
	@test -s "$@"

../output/regime_score_comparison.csv: ../output/regime_scores.rds
	@test -s "$@"

../output/regime_period_scale.csv: ../output/regime_scores.rds
	@test -s "$@"

../output/regime_density_results.csv: \
	estimate_regime_density.R \
	../output/regime_scores.rds \
	../output/score_leaveout_data.rds \
	../input/new_construction_analysis_data.csv | ../output
	$(R) $<

../output/regime_classification_stability.csv: ../output/regime_density_results.csv
	@test -s "$@"

../output/regime_flip_contributors.csv: ../output/regime_density_results.csv
	@test -s "$@"

../output/regime_score_coverage.csv: ../output/regime_density_results.csv
	@test -s "$@"

../output/regime_density_rd_dated_common.pdf: \
	plot_regime_density.R \
	../output/regime_scores.rds \
	../output/score_leaveout_data.rds \
	../input/new_construction_analysis_data.csv | ../output
	$(R) $<

../output/regime_density_rd_pooled_full.pdf: ../output/regime_density_rd_dated_common.pdf
	@test -s "$@"

../output/regime_density_rd_pooled_common.pdf: ../output/regime_density_rd_dated_common.pdf
	@test -s "$@"

../output/ward_36_geography_change.csv: \
	check_ward_36_change.R \
	../../../../data_raw/Wards_2014.geojson \
	../../../../data_raw/Wards_2015.geojson | ../output
	$(R) $<

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
