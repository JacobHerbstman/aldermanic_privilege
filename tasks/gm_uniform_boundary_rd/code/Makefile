SHELL := bash
include ../../shell_functions.make

LEGACY_OUTPUTS := \
	../output/gm_uniform_boundary_rd_detail_all.csv \
	../output/gm_uniform_boundary_rd_detail_pruned.csv \
	../output/gm_uniform_boundary_rd_table_all.csv \
	../output/gm_uniform_boundary_rd_table_pruned.csv \
	../output/gm_uniform_boundary_rd_table_all.tex \
	../output/gm_uniform_boundary_rd_table_pruned.tex \
	../output/gm_uniform_distance_gradients_bw1000m_all.pdf \
	../output/gm_uniform_distance_gradients_bw1000m_pruned.pdf \
	../output/gm_uniform_distance_gradients_bw1000m_all_bins.csv \
	../output/gm_uniform_distance_gradients_bw1000m_pruned_bins.csv

INPUTS := \
	../input/sales_with_hedonics.parquet \
	../input/rent_with_ward_distances_full.parquet \
	../input/confounded_pair_era_flags.csv \
	../input/parcels_with_ward_distances.csv

BAYER_RUN_OUTPUTS := \
	../output/bayer_kulka_construction/gm_bayer_kulka_construction_inference_detail.csv \
	../output/bayer_kulka_construction/gm_bayer_kulka_construction_plot_detail.csv

BAYER_SELECT_OUTPUTS := \
	../output/bayer_kulka_construction/gm_bayer_kulka_construction_scorecard.csv \
	../output/bayer_kulka_construction/gm_bayer_kulka_construction_winners.csv \
	../output/bayer_kulka_construction/gm_bayer_kulka_construction_summary.md \
	../output/bayer_kulka_construction/plots/gm_bayer_kulka_main_stacked_pruned_far_dupac.pdf

GM_PRICES_RENTS_OUTPUTS := \
	../output/bayer_kulka_prices_rents/gm_prices_rents_inference_detail.csv \
	../output/bayer_kulka_prices_rents/gm_prices_rents_plot_detail.csv \
	../output/bayer_kulka_prices_rents/gm_prices_rents_scorecard.csv \
	../output/bayer_kulka_prices_rents/gm_prices_rents_input_qc.csv \
	../output/bayer_kulka_prices_rents/gm_prices_rents_summary.md \
	../output/bayer_kulka_prices_rents/plots/gm_prices_rents_main_stacked_pruned_log_additive_hedonic_bw500ft_allbins.pdf \
	../output/bayer_kulka_prices_rents/plots/gm_prices_rents_main_stacked_pruned_log_additive_hedonic_bw1000ft_allbins.pdf

LEGACY_STAMP := ../output/.gm_uniform_boundary_rd_legacy.stamp
BAYER_RUN_STAMP := ../output/bayer_kulka_construction/.gm_bayer_kulka_run.stamp
BAYER_SELECT_STAMP := ../output/bayer_kulka_construction/.gm_bayer_kulka_select.stamp
GM_PRICES_RENTS_RUN_STAMP := ../output/bayer_kulka_prices_rents/.gm_prices_rents_run.stamp
GM_PRICES_RENTS_ASSEMBLE_STAMP := ../output/bayer_kulka_prices_rents/.gm_prices_rents_assemble.stamp

all: $(LEGACY_OUTPUTS) $(BAYER_RUN_OUTPUTS) $(BAYER_SELECT_OUTPUTS) $(GM_PRICES_RENTS_OUTPUTS)

$(LEGACY_STAMP): run_gm_uniform_boundary_rd.R ../input/sales_with_hedonics.parquet ../input/confounded_pair_era_flags.csv | ../output
	Rscript run_gm_uniform_boundary_rd.R
	touch "$@"

$(LEGACY_OUTPUTS): $(LEGACY_STAMP)

$(BAYER_RUN_STAMP): run_gm_bayer_kulka_construction_np.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv | ../output
	Rscript run_gm_bayer_kulka_construction_np.R
	touch "$@"

$(BAYER_RUN_OUTPUTS): $(BAYER_RUN_STAMP)

$(BAYER_SELECT_STAMP): select_gm_bayer_kulka_winners.R $(BAYER_RUN_OUTPUTS) | ../output
	Rscript select_gm_bayer_kulka_winners.R
	touch "$@"

$(BAYER_SELECT_OUTPUTS): $(BAYER_SELECT_STAMP)

$(GM_PRICES_RENTS_RUN_STAMP): run_gm_prices_rents_spec.R ../input/sales_with_hedonics.parquet ../input/rent_with_ward_distances_full.parquet ../input/confounded_pair_era_flags.csv | ../output
	mkdir -p ../output/bayer_kulka_prices_rents/spec_inference ../output/bayer_kulka_prices_rents/spec_plot ../output/bayer_kulka_prices_rents/plots
	rm -f ../output/bayer_kulka_prices_rents/spec_inference/*.csv
	rm -f ../output/bayer_kulka_prices_rents/spec_plot/*.csv
	rm -f ../output/bayer_kulka_prices_rents/plots/gm_prices_rents_*.pdf
	rm -f ../output/bayer_kulka_prices_rents/qc_unmapped_pair_era_*.csv
	@spec_grid=../output/bayer_kulka_prices_rents/.spec_grid.tsv.tmp; \
	: > $$spec_grid; \
	for dataset in sales rent; do \
		for transform in level log; do \
			for sample_tag in all pruned; do \
				for bw_ft in 500 1000; do \
					for fe_structure in additive interacted; do \
						for controls_tag in no_hedonic hedonic; do \
							echo "$$dataset $$transform $$sample_tag $$bw_ft $$fe_structure $$controls_tag" >> $$spec_grid; \
						done; \
					done; \
				done; \
			done; \
		done; \
	done; \
	cat $$spec_grid | xargs -n 6 -P 6 bash -lc 'echo "Running prices/rents spec: $$1 $$2 $$3 $$4 $$5 $$6"; Rscript run_gm_prices_rents_spec.R "$$1" "$$2" "$$3" "$$4" "$$5" "$$6"' _; \
	rm -f $$spec_grid
	touch "$@"

$(GM_PRICES_RENTS_ASSEMBLE_STAMP): assemble_gm_prices_rents_results.R $(GM_PRICES_RENTS_RUN_STAMP) | ../output
	Rscript assemble_gm_prices_rents_results.R
	touch "$@"

$(GM_PRICES_RENTS_OUTPUTS): $(GM_PRICES_RENTS_ASSEMBLE_STAMP)

../input/sales_with_hedonics.parquet: ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	ln -sf "$<" "$@"

../input/rent_with_ward_distances_full.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUTS)

include ../../generic.make
