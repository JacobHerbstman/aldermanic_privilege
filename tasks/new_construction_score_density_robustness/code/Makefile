SHELL := bash
include ../../shell_functions.make

BASELINE_SPEC := ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH

all: \
	../output/alderman_uncertainty_index_new_construction.csv \
	../output/score_variant_metadata_new_construction.csv \
	../output/score_variant_stage1_terms_new_construction.csv \
	../output/stage1_regression_new_construction.tex \
	../output/stage2_regression_new_construction.tex \
	../output/uncertainty_index_new_construction.pdf \
	../output/new_construction_processing_time_summary.csv \
	../output/new_construction_processing_time_summary.tex \
	../output/new_construction_alderman_processing_times.csv \
	../output/new_construction_alderman_processing_time_density.pdf \
	../output/alderman_uncertainty_index_new_construction_demolition.csv \
	../output/score_variant_metadata_new_construction_demolition.csv \
	../output/score_variant_stage1_terms_new_construction_demolition.csv \
	../output/stage1_regression_new_construction_demolition.tex \
	../output/stage2_regression_new_construction_demolition.tex \
	../output/uncertainty_index_new_construction_demolition.pdf \
	../output/new_construction_demolition_processing_time_summary.csv \
	../output/new_construction_demolition_processing_time_summary.tex \
	../output/new_construction_demolition_alderman_processing_times.csv \
	../output/new_construction_demolition_alderman_processing_time_density.pdf \
	../output/score_comparison_new_construction_vs_baseline.csv \
	../output/score_comparison_new_construction_vs_baseline_summary.csv \
	../output/score_comparison_new_construction_vs_baseline_summary.tex \
	../output/score_comparison_new_construction_vs_baseline_scatter.pdf \
	../output/score_comparison_new_construction_demolition_vs_baseline.csv \
	../output/score_comparison_new_construction_demolition_vs_baseline_summary.csv \
	../output/score_comparison_new_construction_demolition_vs_baseline_summary.tex \
	../output/score_comparison_new_construction_demolition_vs_baseline_scatter.pdf \
	../output/parcels_with_ward_distances_baseline.csv \
	../output/boundary_distance_summary_baseline.csv \
	../output/parcels_with_ward_distances_new_construction.csv \
	../output/boundary_distance_summary_new_construction.csv \
	../output/parcels_with_ward_distances_new_construction_demolition.csv \
	../output/boundary_distance_summary_new_construction_demolition.csv \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.pdf \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_bins.csv \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_meta.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.pdf \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_bins.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_meta.csv \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.pdf \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_bins.csv \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_meta.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.pdf \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_bins.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_meta.csv \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.tex \
	../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.tex \
	../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.csv \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.tex \
	../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.csv \
	../output/density_score_robustness_comparison.csv \
	../output/density_score_robustness_comparison.tex \
	../output/parcel_sign_agreement_baseline_vs_new_construction.csv \
	../output/density_score_robustness_comparison_new_construction_demolition.csv \
	../output/density_score_robustness_comparison_new_construction_demolition.tex \
	../output/parcel_sign_agreement_baseline_vs_new_construction_demolition.csv

../output/alderman_uncertainty_index_new_construction.csv \
../output/score_variant_metadata_new_construction.csv \
../output/score_variant_stage1_terms_new_construction.csv \
../output/stage1_regression_new_construction.tex \
../output/stage2_regression_new_construction.tex \
../output/uncertainty_index_new_construction.pdf: ../output/.score_new_construction.done

../output/.score_new_construction.done: build_new_construction_score.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript build_new_construction_score.R ../input/permits_for_uncertainty_index.csv ../output/alderman_uncertainty_index_new_construction.csv ../output/score_variant_metadata_new_construction.csv ../output/score_variant_stage1_terms_new_construction.csv ../output/stage1_regression_new_construction.tex ../output/stage2_regression_new_construction.tex ../output/uncertainty_index_new_construction.pdf
	touch "$@"

../output/alderman_uncertainty_index_new_construction_demolition.csv \
../output/score_variant_metadata_new_construction_demolition.csv \
../output/score_variant_stage1_terms_new_construction_demolition.csv \
../output/stage1_regression_new_construction_demolition.tex \
../output/stage2_regression_new_construction_demolition.tex \
../output/uncertainty_index_new_construction_demolition.pdf: ../output/.score_new_construction_demolition.done

../output/.score_new_construction_demolition.done: build_new_construction_score.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript build_new_construction_score.R ../input/permits_for_uncertainty_index.csv ../output/alderman_uncertainty_index_new_construction_demolition.csv ../output/score_variant_metadata_new_construction_demolition.csv ../output/score_variant_stage1_terms_new_construction_demolition.csv ../output/stage1_regression_new_construction_demolition.tex ../output/stage2_regression_new_construction_demolition.tex ../output/uncertainty_index_new_construction_demolition.pdf new_construction_demolition "New construction + demolition" "new_construction,demolition"
	touch "$@"

../output/new_construction_processing_time_summary.csv \
../output/new_construction_processing_time_summary.tex \
../output/new_construction_alderman_processing_times.csv \
../output/new_construction_alderman_processing_time_density.pdf: ../output/.processing_time_summary.done

../output/.processing_time_summary.done: summarize_new_construction_processing_time.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript summarize_new_construction_processing_time.R ../input/permits_for_uncertainty_index.csv ../output/new_construction_processing_time_summary.csv ../output/new_construction_processing_time_summary.tex ../output/new_construction_alderman_processing_times.csv ../output/new_construction_alderman_processing_time_density.pdf
	touch "$@"

../output/new_construction_demolition_processing_time_summary.csv \
../output/new_construction_demolition_processing_time_summary.tex \
../output/new_construction_demolition_alderman_processing_times.csv \
../output/new_construction_demolition_alderman_processing_time_density.pdf: ../output/.processing_time_summary_new_construction_demolition.done

../output/.processing_time_summary_new_construction_demolition.done: summarize_new_construction_processing_time.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript summarize_new_construction_processing_time.R ../input/permits_for_uncertainty_index.csv ../output/new_construction_demolition_processing_time_summary.csv ../output/new_construction_demolition_processing_time_summary.tex ../output/new_construction_demolition_alderman_processing_times.csv ../output/new_construction_demolition_alderman_processing_time_density.pdf new_construction_demolition "New construction + demolition" "new_construction,demolition"
	touch "$@"

../output/score_comparison_new_construction_vs_baseline.csv \
../output/score_comparison_new_construction_vs_baseline_summary.csv \
../output/score_comparison_new_construction_vs_baseline_summary.tex \
../output/score_comparison_new_construction_vs_baseline_scatter.pdf: ../output/.score_comparison.done

../output/.score_comparison.done: compare_new_construction_scores.R ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../output/alderman_uncertainty_index_new_construction.csv | ../output
	Rscript compare_new_construction_scores.R ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../output/alderman_uncertainty_index_new_construction.csv ../output/score_comparison_new_construction_vs_baseline.csv ../output/score_comparison_new_construction_vs_baseline_summary.csv ../output/score_comparison_new_construction_vs_baseline_summary.tex ../output/score_comparison_new_construction_vs_baseline_scatter.pdf
	touch "$@"

../output/score_comparison_new_construction_demolition_vs_baseline.csv \
../output/score_comparison_new_construction_demolition_vs_baseline_summary.csv \
../output/score_comparison_new_construction_demolition_vs_baseline_summary.tex \
../output/score_comparison_new_construction_demolition_vs_baseline_scatter.pdf: ../output/.score_comparison_new_construction_demolition.done

../output/.score_comparison_new_construction_demolition.done: compare_new_construction_scores.R ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../output/alderman_uncertainty_index_new_construction_demolition.csv | ../output
	Rscript compare_new_construction_scores.R ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../output/alderman_uncertainty_index_new_construction_demolition.csv ../output/score_comparison_new_construction_demolition_vs_baseline.csv ../output/score_comparison_new_construction_demolition_vs_baseline_summary.csv ../output/score_comparison_new_construction_demolition_vs_baseline_summary.tex ../output/score_comparison_new_construction_demolition_vs_baseline_scatter.pdf new_construction_demolition "New construction + demolition"
	touch "$@"

../output/parcels_with_ward_distances_baseline.csv \
../output/boundary_distance_summary_baseline.csv: ../output/.baseline_merge.done

../output/.baseline_merge.done: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../output
	PARCELS_INPUT_PATH=../input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances_baseline.csv MERGE_SUMMARY_OUTPUT_PATH=../output/boundary_distance_summary_baseline.csv Rscript ../../merge_in_scores/code/merge_scores.R ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv uncertainty_index
	touch "$@"

../output/parcels_with_ward_distances_new_construction.csv \
../output/boundary_distance_summary_new_construction.csv: ../output/.new_construction_merge.done

../output/.new_construction_merge.done: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../output/alderman_uncertainty_index_new_construction.csv | ../output
	PARCELS_INPUT_PATH=../input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances_new_construction.csv MERGE_SUMMARY_OUTPUT_PATH=../output/boundary_distance_summary_new_construction.csv Rscript ../../merge_in_scores/code/merge_scores.R ../output/alderman_uncertainty_index_new_construction.csv uncertainty_index
	touch "$@"

../output/parcels_with_ward_distances_new_construction_demolition.csv \
../output/boundary_distance_summary_new_construction_demolition.csv: ../output/.new_construction_demolition_merge.done

../output/.new_construction_demolition_merge.done: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../output/alderman_uncertainty_index_new_construction_demolition.csv | ../output
	PARCELS_INPUT_PATH=../input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances_new_construction_demolition.csv MERGE_SUMMARY_OUTPUT_PATH=../output/boundary_distance_summary_new_construction_demolition.csv Rscript ../../merge_in_scores/code/merge_scores.R ../output/alderman_uncertainty_index_new_construction_demolition.csv uncertainty_index
	touch "$@"

../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf \
../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv \
../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv: ../output/.rd_far_baseline.done

../output/.rd_far_baseline.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_baseline.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_baseline.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf slope
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf \
../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv \
../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv: ../output/.rd_dupac_baseline.done

../output/.rd_dupac_baseline.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_baseline.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_baseline.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf slope
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.pdf \
../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_bins.csv \
../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_meta.csv: ../output/.rd_far_new_construction.done

../output/.rd_far_new_construction.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_new_construction.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_new_construction.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.pdf slope
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_bins.csv
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.pdf \
../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_bins.csv \
../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_meta.csv: ../output/.rd_dupac_new_construction.done

../output/.rd_dupac_new_construction.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_new_construction.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_new_construction.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.pdf slope
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_bins.csv
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.pdf \
../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_bins.csv \
../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_meta.csv: ../output/.rd_far_new_construction_demolition.done

../output/.rd_far_new_construction_demolition.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_new_construction_demolition.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_new_construction_demolition.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.pdf slope
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_bins.csv
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.pdf \
../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_bins.csv \
../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_meta.csv: ../output/.rd_dupac_new_construction_demolition.done

../output/.rd_dupac_new_construction_demolition.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_new_construction_demolition.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_new_construction_demolition.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.pdf slope
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_bins.csv
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_meta.csv
	touch "$@"

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.tex \
../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv: ../output/.fe_baseline.done

../output/.fe_baseline.done: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_baseline.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_baseline.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
	touch "$@"

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.tex \
../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.csv: ../output/.fe_new_construction.done

../output/.fe_new_construction.done: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_new_construction.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_new_construction.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.csv CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
	touch "$@"

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.tex \
../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.csv: ../output/.fe_new_construction_demolition.done

../output/.fe_new_construction_demolition.done: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_new_construction_demolition.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_new_construction_demolition.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.csv CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
	touch "$@"

../output/density_score_robustness_comparison.csv \
../output/density_score_robustness_comparison.tex \
../output/parcel_sign_agreement_baseline_vs_new_construction.csv: ../output/.density_summary.done

../output/.density_summary.done: summarize_new_construction_density_robustness.R ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_meta.csv ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_meta.csv ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.csv ../output/parcels_with_ward_distances_baseline.csv ../output/parcels_with_ward_distances_new_construction.csv | ../output
	Rscript summarize_new_construction_density_robustness.R ../output
	touch "$@"

../output/density_score_robustness_comparison_new_construction_demolition.csv \
../output/density_score_robustness_comparison_new_construction_demolition.tex \
../output/parcel_sign_agreement_baseline_vs_new_construction_demolition.csv: ../output/.density_summary_new_construction_demolition.done

../output/.density_summary_new_construction_demolition.done: summarize_new_construction_density_robustness.R ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_meta.csv ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition_meta.csv ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.csv ../output/parcels_with_ward_distances_baseline.csv ../output/parcels_with_ward_distances_new_construction_demolition.csv | ../output
	Rscript summarize_new_construction_density_robustness.R ../output new_construction_demolition "New construction + demolition" ../output/density_score_robustness_comparison_new_construction_demolition.csv ../output/density_score_robustness_comparison_new_construction_demolition.tex ../output/parcel_sign_agreement_baseline_vs_new_construction_demolition.csv
	touch "$@"

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf "$<" "$@"

../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../input
	ln -sf "$<" "$@"

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv

include ../../generic.make
