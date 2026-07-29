all: \
	../output/area_weighted_main_results.csv \
	../output/area_weighted_score_comparison.csv \
	../output/area_weighted_score_movements.csv \
	../output/density_rd_area_weighted_4panel.pdf \
	../output/density_rd_area_weighted_4panel.png

../output/ward_controls_area_weighted_2000_2023.csv: build_area_weighted_ward_controls.R ../input/ward_panel.gpkg | ../output
	$(R) $<

../output/area_weighting_coverage.csv: ../output/ward_controls_area_weighted_2000_2023.csv
	@test -s "$@"

../output/permits_for_uncertainty_index_area_weighted.csv: build_area_weighted_scores.R ../../../_lib/alderman_uncertainty_helpers.R ../output/ward_controls_area_weighted_2000_2023.csv ../input/permits_for_uncertainty_index_centroid.csv | ../output
	$(R) $<

../output/alderman_scores_area_weighted_through2014.csv \
../output/alderman_scores_area_weighted_through2022.csv \
../output/area_weighted_scores.csv \
../output/area_weighted_stage1_terms.csv: ../output/permits_for_uncertainty_index_area_weighted.csv
	@test -s "$@"

../output/current_income_score_results.csv: ../../income_score_current_results/code/run_current_income_score_results.R ../../../_lib/alderman_uncertainty_helpers.R ../../../_lib/border_pair_helpers.R ../input/permits_for_uncertainty_index.csv ../input/production_score_through2014.csv ../input/production_score_through2022.csv ../input/parcels_with_ward_distances.csv ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $<

../output/area_weighted_main_results.csv: summarize_area_weighting.R ../output/current_income_score_results.csv ../input/centroid_main_results.csv ../input/centroid_score_through2014.csv ../input/centroid_score_through2022.csv ../output/alderman_scores_area_weighted_through2014.csv ../output/alderman_scores_area_weighted_through2022.csv | ../output
	$(R) $<

../output/area_weighted_score_comparison.csv \
../output/area_weighted_score_movements.csv: ../output/area_weighted_main_results.csv
	@test -s "$@"

../output/density_rd_area_weighted_4panel.pdf: plot_area_weighted_density_rd.R ../../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../output/alderman_scores_area_weighted_through2022.csv | ../output
	$(R) $<

../output/density_rd_area_weighted_4panel.png \
../output/density_rd_area_weighted_visual_estimates.csv: ../output/density_rd_area_weighted_4panel.pdf
	@test -s "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permits_for_uncertainty_index_centroid.csv: ../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permits_for_uncertainty_index.csv: ../output/permits_for_uncertainty_index_area_weighted.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/production_score_through2014.csv: ../output/alderman_scores_area_weighted_through2014.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/production_score_through2022.csv: ../output/alderman_scores_area_weighted_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/centroid_main_results.csv: ../../income_score_current_results/output/current_income_score_results.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/centroid_score_through2014.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/centroid_score_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rental_rd_characteristics_panel_bw500.parquet: ../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/ward_panel.gpkg \
	../input/permits_for_uncertainty_index_centroid.csv \
	../input/permits_for_uncertainty_index.csv \
	../input/production_score_through2014.csv \
	../input/production_score_through2022.csv \
	../input/centroid_main_results.csv \
	../input/centroid_score_through2014.csv \
	../input/centroid_score_through2022.csv \
	../input/parcels_with_ward_distances.csv \
	../input/rental_rd_characteristics_panel_bw500.parquet \
	../input/sales_with_hedonics_amenities.parquet \
	../input/permit_block_year_panel_2015.parquet

include ../../../shell_functions.make
include ../../../generic.make
