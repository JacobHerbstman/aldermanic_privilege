N_DRAWS := 2000
SEED := 20260719
WORKERS := 8

all: \
	../output/current_income_score_results.csv \
	../output/current_income_score_comparison.csv \
	../output/current_income_score_pair_orderings.csv \
	../output/current_income_score_stage1_terms.csv \
	../output/current_income_scores.csv \
	../output/income_score_map_2022.pdf \
	../output/income_score_stage1_2022.tex \
	../output/income_score_distribution_2022.pdf \
	../output/income_score_predecessor_successor_2022.pdf \
	../output/current_income_score_movements.csv \
	../output/current_income_sales_pair_drivers.csv \
	../output/current_income_density_pair_drivers.csv \
	../output/income_score_draws_through2022_$(N_DRAWS)draws.parquet \
	../output/continuous_price_score_uncertainty_summary_$(N_DRAWS)draws.csv \
	../output/continuous_price_leave_pair_out_summary.csv

../output/current_income_score_results.csv: run_current_income_score_results.R ../../../_lib/alderman_uncertainty_helpers.R ../../../_lib/border_pair_helpers.R ../input/permits_for_uncertainty_index.csv ../input/production_score_through2014.csv ../input/production_score_through2022.csv ../input/parcels_with_ward_distances.csv ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $<

../output/current_income_score_comparison.csv \
../output/current_income_score_pair_orderings.csv \
../output/current_income_score_stage1_terms.csv \
../output/current_income_scores.csv: ../output/current_income_score_results.csv
	@test -s "$@"

../output/income_score_map_2022.pdf: build_income_score_map.R \
	../output/current_income_scores.csv \
	../input/chicago_alderman_panel.csv \
	../input/ward_panel.gpkg | ../output
	$(R) $<

../output/income_score_stage1_2022.tex: \
	build_income_score_stage1_table.R \
	../../../_lib/alderman_uncertainty_helpers.R \
	../input/permits_for_uncertainty_index.csv | ../output
	$(R) $<

../output/income_score_distribution_2022.pdf: \
	build_income_score_distribution.R \
	../output/current_income_scores.csv | ../output
	$(R) $<

../output/income_score_predecessor_successor_2022.pdf: \
	build_income_score_predecessor_successor.R \
	../output/current_income_scores.csv \
	../input/chicago_alderman_panel.csv | ../output
	$(R) $<

../output/current_income_score_movements.csv: analyze_income_score_drivers.R ../output/current_income_scores.csv ../input/sales_with_hedonics_amenities.parquet ../input/parcels_with_ward_distances.csv | ../output
	$(R) $<

../output/current_income_sales_pair_drivers.csv \
../output/current_income_density_pair_drivers.csv: ../output/current_income_score_movements.csv
	@test -s "$@"

../output/income_score_draws_through2022_$(N_DRAWS)draws.parquet: bootstrap_income_scores.R ../../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv ../output/current_income_scores.csv | ../output
	$(R) $< $(N_DRAWS) $(SEED) $(WORKERS)

../output/income_score_draws_through2022_$(N_DRAWS)draws_summary.csv: ../output/income_score_draws_through2022_$(N_DRAWS)draws.parquet
	@test -s "$@"

../output/continuous_price_score_uncertainty_summary_$(N_DRAWS)draws.csv: propagate_continuous_price_score_uncertainty.R ../input/production_score_draws_through2022_$(N_DRAWS)draws.parquet ../output/income_score_draws_through2022_$(N_DRAWS)draws.parquet ../output/current_income_scores.csv ../output/current_income_score_results.csv ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet | ../output
	$(R) $< $(N_DRAWS)

../output/continuous_price_score_uncertainty_$(N_DRAWS)draws.csv: ../output/continuous_price_score_uncertainty_summary_$(N_DRAWS)draws.csv
	@test -s "$@"

../output/continuous_price_leave_pair_out_summary.csv: run_continuous_price_leave_pair_out.R ../output/current_income_scores.csv ../output/current_income_score_results.csv ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet | ../output
	$(R) $< $(WORKERS)

../output/continuous_price_leave_pair_out.csv: ../output/continuous_price_leave_pair_out_summary.csv
	@test -s "$@"

../input/permits_for_uncertainty_index.csv: ../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/production_score_through2014.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/production_score_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rental_rd_characteristics_panel_bw500.parquet: ../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/production_score_draws_through2022_$(N_DRAWS)draws.parquet: ../../generated_score_uncertainty/output/score_draws_through2022_$(N_DRAWS)draws.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv ../input/production_score_through2014.csv ../input/production_score_through2022.csv ../input/parcels_with_ward_distances.csv ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permit_block_year_panel_2015.parquet ../input/chicago_alderman_panel.csv ../input/ward_panel.gpkg ../input/production_score_draws_through2022_$(N_DRAWS)draws.parquet

include ../../../shell_functions.make
include ../../../generic.make
