all: \
	../output/union_donations_permit_correlations.csv \
	../output/union_donations_permit_summary_stats.csv \
	../output/union_donations_permit_join.csv \
	../output/union_donations_permit_top_bottom.csv \
	../output/union_donations_permit_unmatched_aldermen.csv \
	../output/union_share_processing_time_scatter.png \
	../output/union_share_processing_time_scatter_ytrimmed.png \
	../output/union_share_permit_counts_scatter.png \
	../output/union_share_permit_counts_scatter_ytrimmed.png \
	../output/union_share_units_density_scatter.png \
	../output/union_share_units_density_scatter_ytrimmed.png \
	../output/union_share_stringency_score_scatter.pdf \
	../output/union_share_stringency_score_scatter.png \
	../output/union_share_stringency_score_join.csv \
	../output/union_share_stringency_score_regression.csv \
	../output/construction_trades_share_stringency_score_scatter.pdf \
	../output/construction_trades_share_stringency_score_scatter.png \
	../output/construction_trades_share_stringency_score_join.csv \
	../output/construction_trades_share_stringency_score_regression.csv \
	../output/construction_trades_share_score_binscatter.pdf \
	../output/construction_trades_share_score_binscatter.png \
	../output/construction_trades_share_score_bins.csv \
	../output/construction_trades_share_score_rank_rank.pdf \
	../output/construction_trades_share_score_rank_rank.png \
	../output/construction_trades_share_score_rank_regression.csv \
	../output/construction_trades_score_extensive_margin.pdf \
	../output/construction_trades_score_extensive_margin.png \
	../output/construction_trades_score_extensive_margin.csv \
	../output/construction_trades_log_dollars_score_scatter.pdf \
	../output/construction_trades_log_dollars_score_scatter.png \
	../output/construction_trades_score_robust_slopes.csv \
	../output/construction_trades_score_leaveout_checks.csv \
	../output/construction_trades_share_score_labeled_scatter.pdf \
	../output/construction_trades_share_score_labeled_scatter.png \
	../output/construction_trades_residualized_join.csv \
	../output/construction_trades_residualized_correlations.csv \
	../output/construction_trades_residualized_leaveout_checks.csv \
	../output/construction_trades_residualized_ppml.csv \
	../output/construction_trades_residualized_ppml_effects.pdf \
	../output/construction_trades_residualized_ppml_effects.png \
	../output/construction_trades_residualized_ppml_adjusted_rates.pdf \
	../output/construction_trades_residualized_ppml_adjusted_rates.png \
	../output/construction_trades_residualized_score_binscatter.pdf \
	../output/construction_trades_residualized_score_binscatter.png \
	../output/construction_trades_residualized_score_rank_rank.pdf \
	../output/construction_trades_residualized_score_rank_rank.png \
	../output/construction_trades_residualized_score_labeled_scatter.pdf \
	../output/construction_trades_residualized_score_labeled_scatter.png \
	../output/construction_trades_residualized_permit_count_scatter.pdf \
	../output/construction_trades_residualized_permit_count_scatter.png \
	../output/construction_trades_residualized_permit_count_binscatter.pdf \
	../output/construction_trades_residualized_permit_count_binscatter.png \
	../output/construction_trades_share_raw_correlations.csv \
	../output/construction_trades_share_processing_time_scatter.pdf \
	../output/construction_trades_share_processing_time_scatter.png \
	../output/construction_trades_share_permit_counts_scatter.pdf \
	../output/construction_trades_share_permit_counts_scatter.png \
	../output/construction_trades_share_log_permit_count_correlations.csv \
	../output/construction_trades_share_log_permit_counts_scatter.pdf \
	../output/construction_trades_share_log_permit_counts_scatter.png \
	../output/construction_trades_share_units_density_scatter.pdf \
	../output/construction_trades_share_units_density_scatter.png \
	../output/union_share_processing_time_scatter.pdf \
	../output/union_share_processing_time_scatter_ytrimmed.pdf \
	../output/union_share_permit_counts_scatter.pdf \
	../output/union_share_permit_counts_scatter_ytrimmed.pdf \
	../output/union_share_units_density_scatter.pdf \
	../output/union_share_units_density_scatter_ytrimmed.pdf

../output/union_donations_permit_correlations.csv: build_union_permit_correlations.R \
	../input/alderman_union_donations_main.csv \
	../input/permits_for_uncertainty_index.csv \
	../input/geocoded_residential_data.gpkg \
	../input/ward_panel.gpkg \
	../input/chicago_alderman_panel.csv \
	../input/ward_controls_2000_2023.csv \
	../input/alderman_cycle_union_category_aggregates_reviewed_named_recipient.csv \
	../input/fred_cpi_cuura207sa0.csv \
	../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv \
	../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through202604.csv | ../output
	Rscript $<

../output/union_donations_permit_summary_stats.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_donations_permit_join.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_donations_permit_top_bottom.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_donations_permit_unmatched_aldermen.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_processing_time_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_processing_time_scatter_ytrimmed.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_permit_counts_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_permit_counts_scatter_ytrimmed.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_units_density_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_units_density_scatter_ytrimmed.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_stringency_score_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_stringency_score_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_stringency_score_join.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_stringency_score_regression.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_stringency_score_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_stringency_score_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_stringency_score_join.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_stringency_score_regression.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_score_binscatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_score_binscatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_score_bins.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_score_rank_rank.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_score_rank_rank.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_score_rank_regression.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_score_extensive_margin.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_score_extensive_margin.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_score_extensive_margin.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_log_dollars_score_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_log_dollars_score_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_score_robust_slopes.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_score_leaveout_checks.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_score_labeled_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_score_labeled_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_join.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_correlations.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_leaveout_checks.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_ppml.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_ppml_effects.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_ppml_effects.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_ppml_adjusted_rates.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_ppml_adjusted_rates.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_score_binscatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_score_binscatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_score_rank_rank.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_score_rank_rank.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_score_labeled_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_score_labeled_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_permit_count_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_permit_count_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_permit_count_binscatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_residualized_permit_count_binscatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_raw_correlations.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_processing_time_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_processing_time_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_permit_counts_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_permit_counts_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_log_permit_count_correlations.csv: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_log_permit_counts_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_log_permit_counts_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_units_density_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/construction_trades_share_units_density_scatter.png: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_processing_time_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_processing_time_scatter_ytrimmed.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_permit_counts_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_permit_counts_scatter_ytrimmed.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_units_density_scatter.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../output/union_share_units_density_scatter_ytrimmed.pdf: ../output/union_donations_permit_correlations.csv
	@test -s "$@"

../input/alderman_union_donations_main.csv: ../../../alderman_union_candidate_aggregates_cycles/output/alderman_union_donations_main.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/permits_for_uncertainty_index.csv: ../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/geocoded_residential_data.gpkg: ../../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_controls_2000_2023.csv: ../../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_cycle_union_category_aggregates_reviewed_named_recipient.csv: ../../../alderman_union_candidate_aggregates_cycles/output/alderman_cycle_union_category_aggregates_reviewed_named_recipient.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/fred_cpi_cuura207sa0.csv: ../../../download_fred_cpi/output/fred_cpi_cuura207sa0.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through202604.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through202604.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/alderman_union_donations_main.csv \
	../input/permits_for_uncertainty_index.csv \
	../input/geocoded_residential_data.gpkg \
	../input/ward_panel.gpkg \
	../input/chicago_alderman_panel.csv \
	../input/ward_controls_2000_2023.csv \
	../input/alderman_cycle_union_category_aggregates_reviewed_named_recipient.csv \
	../input/fred_cpi_cuura207sa0.csv \
	../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv \
	../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through202604.csv

include ../../../generic.make
