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
	../output/renovation_text_classification_restricted_renovation.csv \
	../output/renovation_text_classification_summary_restricted_renovation.csv \
	../output/renovation_text_classification_porch_keyword_summary_restricted_renovation.csv \
	../output/renovation_text_classification_mixed_job_summary_restricted_renovation.csv \
	../output/renovation_text_classification_dropped_description_review_restricted_renovation.csv \
	../output/permits_for_uncertainty_index_restricted_renovation.csv \
	../output/alderman_uncertainty_index_restricted_renovation.csv \
	../output/score_variant_metadata_restricted_renovation.csv \
	../output/score_variant_stage1_terms_restricted_renovation.csv \
	../output/stage1_regression_restricted_renovation.tex \
	../output/stage2_regression_restricted_renovation.tex \
	../output/uncertainty_index_restricted_renovation.pdf \
	../output/score_comparison_restricted_renovation_vs_baseline.csv \
	../output/score_comparison_restricted_renovation_vs_baseline_summary.csv \
	../output/score_comparison_restricted_renovation_vs_baseline_summary.tex \
	../output/score_comparison_restricted_renovation_vs_baseline_scatter.pdf \
	../output/score_rankings_restricted_renovation.tex \
	../output/parcels_with_ward_distances_baseline.csv \
	../output/boundary_distance_summary_baseline.csv \
	../output/parcels_with_ward_distances_new_construction.csv \
	../output/boundary_distance_summary_new_construction.csv \
	../output/parcels_with_ward_distances_new_construction_demolition.csv \
	../output/boundary_distance_summary_new_construction_demolition.csv \
	../output/parcels_with_ward_distances_restricted_renovation.csv \
	../output/boundary_distance_summary_restricted_renovation.csv \
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
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.pdf \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_bins.csv \
	../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.pdf \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_bins.csv \
	../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.tex \
	../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.tex \
	../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction.csv \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.tex \
	../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_new_construction_demolition.csv \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.tex \
	../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.csv \
	../output/density_score_robustness_comparison.csv \
	../output/density_score_robustness_comparison.tex \
	../output/parcel_sign_agreement_baseline_vs_new_construction.csv \
	../output/density_score_robustness_comparison_new_construction_demolition.csv \
	../output/density_score_robustness_comparison_new_construction_demolition.tex \
	../output/parcel_sign_agreement_baseline_vs_new_construction_demolition.csv \
	../output/density_score_robustness_comparison_restricted_renovation.csv \
	../output/density_score_robustness_comparison_restricted_renovation.tex \
	../output/parcel_sign_agreement_baseline_vs_restricted_renovation.csv

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

../output/alderman_uncertainty_index_no_renovation.csv \
../output/score_variant_metadata_no_renovation.csv \
../output/score_variant_stage1_terms_no_renovation.csv \
../output/stage1_regression_no_renovation.tex \
../output/stage2_regression_no_renovation.tex \
../output/uncertainty_index_no_renovation.pdf: ../output/.score_no_renovation.done

../output/.score_no_renovation.done: build_new_construction_score.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript build_new_construction_score.R ../input/permits_for_uncertainty_index.csv ../output/alderman_uncertainty_index_no_renovation.csv ../output/score_variant_metadata_no_renovation.csv ../output/score_variant_stage1_terms_no_renovation.csv ../output/stage1_regression_no_renovation.tex ../output/stage2_regression_no_renovation.tex ../output/uncertainty_index_no_renovation.pdf no_renovation "All high-discretion except renovation" "new_construction,demolition,porch,reinstate"
	touch "$@"

../output/renovation_text_classification_restricted_renovation.csv \
../output/renovation_text_classification_summary_restricted_renovation.csv \
../output/renovation_text_classification_porch_keyword_summary_restricted_renovation.csv \
../output/renovation_text_classification_mixed_job_summary_restricted_renovation.csv \
../output/renovation_text_classification_dropped_description_review_restricted_renovation.csv \
../output/permits_for_uncertainty_index_restricted_renovation.csv: ../output/.classify_restricted_renovation.done

../output/.classify_restricted_renovation.done: classify_restricted_renovation_text.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv ../input/building_permits_clean.gpkg | ../output
	Rscript classify_restricted_renovation_text.R ../input/permits_for_uncertainty_index.csv ../input/building_permits_clean.gpkg ../output/renovation_text_classification_restricted_renovation.csv ../output/renovation_text_classification_summary_restricted_renovation.csv ../output/renovation_text_classification_porch_keyword_summary_restricted_renovation.csv ../output/renovation_text_classification_mixed_job_summary_restricted_renovation.csv ../output/renovation_text_classification_dropped_description_review_restricted_renovation.csv ../output/permits_for_uncertainty_index_restricted_renovation.csv 2022
	touch "$@"

../output/alderman_uncertainty_index_restricted_renovation.csv \
../output/score_variant_metadata_restricted_renovation.csv \
../output/score_variant_stage1_terms_restricted_renovation.csv \
../output/stage1_regression_restricted_renovation.tex \
../output/stage2_regression_restricted_renovation.tex \
../output/uncertainty_index_restricted_renovation.pdf: ../output/.score_restricted_renovation.done

../output/.score_restricted_renovation.done: build_restricted_renovation_score.R ../../_lib/alderman_uncertainty_helpers.R ../output/permits_for_uncertainty_index_restricted_renovation.csv | ../output
	Rscript build_restricted_renovation_score.R ../output/permits_for_uncertainty_index_restricted_renovation.csv ../output/alderman_uncertainty_index_restricted_renovation.csv ../output/score_variant_metadata_restricted_renovation.csv ../output/score_variant_stage1_terms_restricted_renovation.csv ../output/stage1_regression_restricted_renovation.tex ../output/stage2_regression_restricted_renovation.tex ../output/uncertainty_index_restricted_renovation.pdf restricted_renovation "Restricted renovation"
	touch "$@"

../output/alderman_uncertainty_index_renovation_new_construction.csv \
../output/score_variant_metadata_renovation_new_construction.csv \
../output/score_variant_stage1_terms_renovation_new_construction.csv \
../output/stage1_regression_renovation_new_construction.tex \
../output/stage2_regression_renovation_new_construction.tex \
../output/uncertainty_index_renovation_new_construction.pdf: ../output/.score_renovation_new_construction.done

../output/.score_renovation_new_construction.done: build_new_construction_score.R ../../_lib/alderman_uncertainty_helpers.R ../input/permits_for_uncertainty_index.csv | ../output
	Rscript build_new_construction_score.R ../input/permits_for_uncertainty_index.csv ../output/alderman_uncertainty_index_renovation_new_construction.csv ../output/score_variant_metadata_renovation_new_construction.csv ../output/score_variant_stage1_terms_renovation_new_construction.csv ../output/stage1_regression_renovation_new_construction.tex ../output/stage2_regression_renovation_new_construction.tex ../output/uncertainty_index_renovation_new_construction.pdf renovation_new_construction "Renovation + new construction" "renovation,new_construction"
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

../output/score_comparison_no_renovation_vs_baseline.csv \
../output/score_comparison_no_renovation_vs_baseline_summary.csv \
../output/score_comparison_no_renovation_vs_baseline_summary.tex \
../output/score_comparison_no_renovation_vs_baseline_scatter.pdf: ../output/.score_comparison_no_renovation.done

../output/.score_comparison_no_renovation.done: compare_new_construction_scores.R ../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../output/alderman_uncertainty_index_no_renovation.csv | ../output
	Rscript compare_new_construction_scores.R ../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../output/alderman_uncertainty_index_no_renovation.csv ../output/score_comparison_no_renovation_vs_baseline.csv ../output/score_comparison_no_renovation_vs_baseline_summary.csv ../output/score_comparison_no_renovation_vs_baseline_summary.tex ../output/score_comparison_no_renovation_vs_baseline_scatter.pdf no_renovation "All high-discretion except renovation"
	touch "$@"

../output/score_comparison_restricted_renovation_vs_baseline.csv \
../output/score_comparison_restricted_renovation_vs_baseline_summary.csv \
../output/score_comparison_restricted_renovation_vs_baseline_summary.tex \
../output/score_comparison_restricted_renovation_vs_baseline_scatter.pdf: ../output/.score_comparison_restricted_renovation.done

../output/.score_comparison_restricted_renovation.done: compare_new_construction_scores.R ../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../output/alderman_uncertainty_index_restricted_renovation.csv | ../output
	Rscript compare_new_construction_scores.R ../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../output/alderman_uncertainty_index_restricted_renovation.csv ../output/score_comparison_restricted_renovation_vs_baseline.csv ../output/score_comparison_restricted_renovation_vs_baseline_summary.csv ../output/score_comparison_restricted_renovation_vs_baseline_summary.tex ../output/score_comparison_restricted_renovation_vs_baseline_scatter.pdf restricted_renovation "Restricted renovation"
	touch "$@"

../output/alderman_uncertainty_index_low_discretion_residualized.csv \
../output/score_variant_metadata_low_discretion_residualized.csv: ../output/.score_low_discretion_residualized.done

../output/.score_low_discretion_residualized.done: build_low_discretion_score.R ../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../output
	Rscript build_low_discretion_score.R ../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../output/alderman_uncertainty_index_low_discretion_residualized.csv ../output/score_variant_metadata_low_discretion_residualized.csv low_discretion_residualized "Residualized low-discretion score"
	touch "$@"

../output/score_comparison_low_discretion_residualized_vs_baseline.csv \
../output/score_comparison_low_discretion_residualized_vs_baseline_summary.csv \
../output/score_comparison_low_discretion_residualized_vs_baseline_summary.tex \
../output/score_comparison_low_discretion_residualized_vs_baseline_scatter.pdf: ../output/.score_comparison_low_discretion_residualized.done

../output/.score_comparison_low_discretion_residualized.done: compare_new_construction_scores.R ../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../output/alderman_uncertainty_index_low_discretion_residualized.csv | ../output
	Rscript compare_new_construction_scores.R ../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../output/alderman_uncertainty_index_low_discretion_residualized.csv ../output/score_comparison_low_discretion_residualized_vs_baseline.csv ../output/score_comparison_low_discretion_residualized_vs_baseline_summary.csv ../output/score_comparison_low_discretion_residualized_vs_baseline_summary.tex ../output/score_comparison_low_discretion_residualized_vs_baseline_scatter.pdf low_discretion_residualized "Residualized low-discretion score"
	touch "$@"

../output/score_rankings_no_renovation.tex: export_score_rankings_tex.R ../output/score_comparison_no_renovation_vs_baseline.csv | ../output
	Rscript export_score_rankings_tex.R ../output/score_comparison_no_renovation_vs_baseline.csv ../output/score_rankings_no_renovation.tex no_renovation

../output/score_rankings_restricted_renovation.tex: export_score_rankings_tex.R ../output/score_comparison_restricted_renovation_vs_baseline.csv | ../output
	Rscript export_score_rankings_tex.R ../output/score_comparison_restricted_renovation_vs_baseline.csv ../output/score_rankings_restricted_renovation.tex restricted_renovation

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

../output/parcels_with_ward_distances_renovation_new_construction.csv \
../output/boundary_distance_summary_renovation_new_construction.csv: ../output/.renovation_new_construction_merge.done

../output/.renovation_new_construction_merge.done: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../output/alderman_uncertainty_index_renovation_new_construction.csv | ../output
	PARCELS_INPUT_PATH=../input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances_renovation_new_construction.csv MERGE_SUMMARY_OUTPUT_PATH=../output/boundary_distance_summary_renovation_new_construction.csv Rscript ../../merge_in_scores/code/merge_scores.R ../output/alderman_uncertainty_index_renovation_new_construction.csv uncertainty_index
	touch "$@"

../output/parcels_with_ward_distances_restricted_renovation.csv \
../output/boundary_distance_summary_restricted_renovation.csv: ../output/.restricted_renovation_merge.done

../output/.restricted_renovation_merge.done: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../output/alderman_uncertainty_index_restricted_renovation.csv | ../output
	PARCELS_INPUT_PATH=../input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances_restricted_renovation.csv MERGE_SUMMARY_OUTPUT_PATH=../output/boundary_distance_summary_restricted_renovation.csv Rscript ../../merge_in_scores/code/merge_scores.R ../output/alderman_uncertainty_index_restricted_renovation.csv uncertainty_index
	touch "$@"

../output/parcels_with_ward_distances_low_discretion_residualized.csv \
../output/boundary_distance_summary_low_discretion_residualized.csv: ../output/.low_discretion_residualized_merge.done

../output/.low_discretion_residualized_merge.done: ../../merge_in_scores/code/merge_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../output/alderman_uncertainty_index_low_discretion_residualized.csv | ../output
	PARCELS_INPUT_PATH=../input/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances_low_discretion_residualized.csv MERGE_SUMMARY_OUTPUT_PATH=../output/boundary_distance_summary_low_discretion_residualized.csv Rscript ../../merge_in_scores/code/merge_scores.R ../output/alderman_uncertainty_index_low_discretion_residualized.csv uncertainty_index
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

../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.pdf \
../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_bins.csv \
../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv: ../output/.rd_far_restricted_renovation.done

../output/.rd_far_restricted_renovation.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_restricted_renovation.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_restricted_renovation.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.pdf slope
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_bins.csv
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.pdf \
../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_bins.csv \
../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv: ../output/.rd_dupac_restricted_renovation.done

../output/.rd_dupac_restricted_renovation.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_restricted_renovation.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_restricted_renovation.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.pdf slope
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_bins.csv
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.pdf \
../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_bins.csv \
../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_meta.csv: ../output/.rd_far_low_discretion_residualized.done

../output/.rd_far_low_discretion_residualized.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_low_discretion_residualized.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_low_discretion_residualized.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.pdf slope
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_bins.csv
	test -f ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.pdf \
../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_bins.csv \
../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_meta.csv: ../output/.rd_dupac_low_discretion_residualized.done

../output/.rd_dupac_low_discretion_residualized.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_low_discretion_residualized.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_low_discretion_residualized.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 500 multifamily zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.pdf slope
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_bins.csv
	test -f ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf \
../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv \
../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv: ../output/.rd_far_bw250_all_baseline.done

../output/.rd_far_bw250_all_baseline.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_baseline.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_baseline.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 250 all zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf slope
	test -f ../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv
	test -f ../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf \
../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv \
../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv: ../output/.rd_dupac_bw250_all_baseline.done

../output/.rd_dupac_bw250_all_baseline.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_baseline.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_baseline.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 250 all zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.pdf slope
	test -f ../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline_bins.csv
	test -f ../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.pdf \
../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_bins.csv \
../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv: ../output/.rd_far_bw250_all_restricted_renovation.done

../output/.rd_far_bw250_all_restricted_renovation.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_restricted_renovation.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_restricted_renovation.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_far TRUE 250 all zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.pdf slope
	test -f ../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_bins.csv
	test -f ../output/rd_fe_plot_log_density_far_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv
	touch "$@"

../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.pdf \
../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_bins.csv \
../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv: ../output/.rd_dupac_bw250_all_restricted_renovation.done

../output/.rd_dupac_bw250_all_restricted_renovation.done: ../../spatial_rd_fe/code/spatial_rd_fe.R ../output/parcels_with_ward_distances_restricted_renovation.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances_restricted_renovation.csv CLUSTER_LEVEL=ward_pair Rscript ../../spatial_rd_fe/code/spatial_rd_fe.R density_dupac TRUE 250 all zonegroup_segment_year_additive ../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.pdf slope
	test -f ../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_bins.csv
	test -f ../output/rd_fe_plot_log_density_dupac_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv
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

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_renovation_new_construction.tex \
../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_renovation_new_construction.csv: ../output/.fe_renovation_new_construction.done

../output/.fe_renovation_new_construction.done: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_renovation_new_construction.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_renovation_new_construction.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_renovation_new_construction.csv CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_renovation_new_construction.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
	touch "$@"

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.tex \
../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.csv: ../output/.fe_restricted_renovation.done

../output/.fe_restricted_renovation.done: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_restricted_renovation.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_restricted_renovation.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.csv CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
	touch "$@"

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.tex \
../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.csv: ../output/.fe_low_discretion_residualized.done

../output/.fe_low_discretion_residualized.done: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_low_discretion_residualized.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_low_discretion_residualized.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.csv CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
	touch "$@"

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_mainoutcomes.tex: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_low_discretion_residualized.csv | ../output
	CLUSTER_LEVEL=ward_pair FE_INPUT_PATH=../output/parcels_with_ward_distances_low_discretion_residualized.csv Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_mainoutcomes.tex "log(density_far)" "log(density_dupac)"

../output/fe_table_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.tex \
../output/fe_summary_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv: ../output/.fe_bw250_all_baseline.done

../output/.fe_bw250_all_baseline.done: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_baseline.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_baseline.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 250 all zonegroup_segment_year_additive ../output/fe_table_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
	touch "$@"

../output/fe_table_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.tex \
../output/fe_summary_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.csv: ../output/.fe_bw250_all_restricted_renovation.done

../output/.fe_bw250_all_restricted_renovation.done: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances_restricted_renovation.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances_restricted_renovation.csv FE_SUMMARY_OUTPUT_PATH=../output/fe_summary_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.csv CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 250 all zonegroup_segment_year_additive ../output/fe_table_bw250_all_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.tex "log(density_far)" "log(density_dupac)" "log(unitscount)"
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

../output/density_score_robustness_comparison_restricted_renovation.csv \
../output/density_score_robustness_comparison_restricted_renovation.tex \
../output/parcel_sign_agreement_baseline_vs_restricted_renovation.csv: ../output/.density_summary_restricted_renovation.done

../output/.density_summary_restricted_renovation.done: summarize_new_construction_density_robustness.R ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation_meta.csv ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_restricted_renovation.csv ../output/parcels_with_ward_distances_baseline.csv ../output/parcels_with_ward_distances_restricted_renovation.csv | ../output
	Rscript summarize_new_construction_density_robustness.R ../output restricted_renovation "Restricted renovation" ../output/density_score_robustness_comparison_restricted_renovation.csv ../output/density_score_robustness_comparison_restricted_renovation.tex ../output/parcel_sign_agreement_baseline_vs_restricted_renovation.csv
	touch "$@"

../output/density_score_robustness_comparison_low_discretion_residualized.csv \
../output/density_score_robustness_comparison_low_discretion_residualized.tex \
../output/parcel_sign_agreement_baseline_vs_low_discretion_residualized.csv: ../output/.density_summary_low_discretion_residualized.done

../output/.density_summary_low_discretion_residualized.done: summarize_new_construction_density_robustness.R ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline_meta.csv ../output/rd_fe_plot_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_meta.csv ../output/rd_fe_plot_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_meta.csv ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../output/fe_summary_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized.csv ../output/parcels_with_ward_distances_baseline.csv ../output/parcels_with_ward_distances_low_discretion_residualized.csv | ../output
	Rscript summarize_new_construction_density_robustness.R ../output low_discretion_residualized "Residualized low-discretion score" ../output/density_score_robustness_comparison_low_discretion_residualized.csv ../output/density_score_robustness_comparison_low_discretion_residualized.tex ../output/parcel_sign_agreement_baseline_vs_low_discretion_residualized.csv
	touch "$@"

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf "$<" "$@"

../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(BASELINE_SPEC).csv | ../input
	ln -sf "$<" "$@"

../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf "$<" "$@"

../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv: ../../within_ward_strictness/output/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	ln -sf "$<" "$@"

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/permits_for_uncertainty_index.csv ../input/alderman_uncertainty_index_$(BASELINE_SPEC).csv ../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../input/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../input/building_permits_clean.gpkg ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv

include ../../generic.make
