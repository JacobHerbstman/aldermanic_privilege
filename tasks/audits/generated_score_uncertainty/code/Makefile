DRAWS := 2000
SEED := 20260719
WORKERS := 8

all: ../output/headline_estimate_uncertainty_$(DRAWS)draws.csv ../output/density_boundary_estimate_uncertainty_$(DRAWS)draws.csv ../output/pair_ordering_probabilities_$(DRAWS)draws.csv ../output/pair_ordering_summary_$(DRAWS)draws.csv ../output/score_draw_diagnostics_summary_$(DRAWS)draws.csv

../output/density_boundary_estimate_uncertainty_$(DRAWS)draws.csv: propagate_density_boundary_score_uncertainty.R ../output/score_draws_through2022_$(DRAWS)draws.parquet ../input/alderman_uncertainty_index_through2022.csv ../input/parcels_with_ward_distances.csv | ../output
	$(R) $< $(DRAWS) $(WORKERS)

../output/density_boundary_estimate_uncertainty_summary_$(DRAWS)draws.csv: ../output/density_boundary_estimate_uncertainty_$(DRAWS)draws.csv
	@test -s "$@"

../output/score_draw_diagnostics_$(DRAWS)draws.csv: propagate_score_uncertainty.R ../output/score_draws_through2022_$(DRAWS)draws.parquet ../output/score_draws_through2014_$(DRAWS)draws.parquet ../input/alderman_uncertainty_index_through2022.csv ../input/alderman_uncertainty_index_through2014.csv ../input/parcels_with_ward_distances.csv ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permit_block_year_panel_2015.parquet | ../output
	$(R) $< $(DRAWS) $(WORKERS)

../output/headline_estimate_uncertainty_$(DRAWS)draws.csv: ../output/score_draw_diagnostics_$(DRAWS)draws.csv
	@test -s "$@"

../output/pair_ordering_probabilities_$(DRAWS)draws.csv: ../output/score_draw_diagnostics_$(DRAWS)draws.csv
	@test -s "$@"

../output/pair_ordering_summary_$(DRAWS)draws.csv: ../output/score_draw_diagnostics_$(DRAWS)draws.csv
	@test -s "$@"

../output/score_draw_diagnostics_summary_$(DRAWS)draws.csv: ../output/score_draw_diagnostics_$(DRAWS)draws.csv
	@test -s "$@"

../output/headline_estimate_uncertainty_summary_$(DRAWS)draws.csv: ../output/headline_estimate_uncertainty_$(DRAWS)draws.csv
	@test -s "$@"

../output/score_draws_through2022_$(DRAWS)draws.parquet: bootstrap_scores.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_through2022.csv | ../output
	$(R) $< $(DRAWS) 2022 $(SEED) $(WORKERS)

../output/score_draws_through2022_$(DRAWS)draws_summary.csv: ../output/score_draws_through2022_$(DRAWS)draws.parquet
	@test -s "$@"

../output/score_draws_through2014_$(DRAWS)draws.parquet: bootstrap_scores.R ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_through2014.csv | ../output
	$(R) $< $(DRAWS) 2014 $(SEED) $(WORKERS)

../output/score_draws_through2014_$(DRAWS)draws_summary.csv: ../output/score_draws_through2014_$(DRAWS)draws.parquet
	@test -s "$@"

../input/permits_for_uncertainty_index.csv: ../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2014.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/rental_rd_characteristics_panel_bw500.parquet: ../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_through2022.csv ../input/alderman_uncertainty_index_through2014.csv ../input/parcels_with_ward_distances.csv ../input/rental_rd_characteristics_panel_bw500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/permit_block_year_panel_2015.parquet

include ../../../shell_functions.make
include ../../../generic.make
