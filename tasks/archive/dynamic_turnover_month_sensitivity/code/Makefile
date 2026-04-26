SHELL := bash

SCORE_SPEC := ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022
SCORE_COLUMN := uncertainty_index
FE_YVARS := 'log(density_far)' 'log(density_dupac)'

all: \
	../output/nonparametric_rd_log_density_far_bw250_all_june.pdf \
	../output/nonparametric_rd_log_density_dupac_bw250_all_june.pdf \
	../output/nonparametric_rd_log_density_far_bw500_multifamily_june.pdf \
	../output/nonparametric_rd_log_density_dupac_bw500_multifamily_june.pdf \
	../output/nonparametric_rd_log_density_far_bw250_all_dynamic_turnover.pdf \
	../output/nonparametric_rd_log_density_dupac_bw250_all_dynamic_turnover.pdf \
	../output/nonparametric_rd_log_density_far_bw500_multifamily_dynamic_turnover.pdf \
	../output/nonparametric_rd_log_density_dupac_bw500_multifamily_dynamic_turnover.pdf \
	../output/fe_table_bw250_all_zonegroup_segment_year_additive_june.tex \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_june.tex \
	../output/fe_table_bw250_all_zonegroup_segment_year_additive_dynamic_turnover.tex \
	../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_dynamic_turnover.tex \
	../output/dynamic_turnover_assignment_summary.csv \
	../output/dynamic_turnover_assignment_by_year.csv \
	../output/dynamic_turnover_main_density_comparison.csv \
	../output/dynamic_turnover_spotcheck_examples.csv

../output/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/code/calculate_distances.R ../../_lib/canonical_geometry_helpers.R ../input/geocoded_residential_data.gpkg ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/block_group_controls.csv ../input/block_group_geometry_2019.gpkg ../input/zoning_data_clean.gpkg | ../output
	SYNTHETIC_CONSTRUCTION_DATE_MODE=dynamic_turnover SYNTHETIC_DATE_DIAGNOSTICS_OUTPUT_PATH=../output/synthetic_month_choice_diagnostics.csv Rscript ../../calculate_ward_boundary_distances/code/calculate_distances.R

../output/parcels_with_geometry.gpkg: ../output/parcels_pre_scores.csv
	test -f "$@"

../output/parcel_geometry_diagnostics.csv: ../output/parcels_pre_scores.csv
	test -f "$@"

../output/synthetic_month_choice_diagnostics.csv: ../output/parcels_pre_scores.csv
	test -f "$@"

../output/parcels_with_ward_distances.csv: ../../merge_in_scores/code/merge_scores.R ../output/parcels_pre_scores.csv ../input/aldermen_uncertainty_scores.csv ../input/parcel_segment_ids.csv | ../output
	PARCELS_INPUT_PATH=../output/parcels_pre_scores.csv SEGMENT_LOOKUP_PATH=../input/parcel_segment_ids.csv MERGE_OUTPUT_PATH=../output/parcels_with_ward_distances.csv MERGE_SUMMARY_OUTPUT_PATH=../output/merge_boundary_distance_summary.csv Rscript ../../merge_in_scores/code/merge_scores.R ../input/aldermen_uncertainty_scores.csv $(SCORE_COLUMN)

../output/rd_summary_june_density_far_bw250_all.csv: ../../nonparametric_rd_density/code/nonparametric_rd_density.R ../input/june_parcels_with_ward_distances.csv | ../output
	RD_INPUT_PATH=../input/june_parcels_with_ward_distances.csv RD_SUMMARY_OUTPUT_PATH=$@ Rscript ../../nonparametric_rd_density/code/nonparametric_rd_density.R density_far TRUE 250 all all ../output/nonparametric_rd_log_density_far_bw250_all_june.pdf feet

../output/nonparametric_rd_log_density_far_bw250_all_june.pdf: ../output/rd_summary_june_density_far_bw250_all.csv
	test -f "$@"

../output/rd_summary_june_density_dupac_bw250_all.csv: ../../nonparametric_rd_density/code/nonparametric_rd_density.R ../input/june_parcels_with_ward_distances.csv | ../output
	RD_INPUT_PATH=../input/june_parcels_with_ward_distances.csv RD_SUMMARY_OUTPUT_PATH=$@ Rscript ../../nonparametric_rd_density/code/nonparametric_rd_density.R density_dupac TRUE 250 all all ../output/nonparametric_rd_log_density_dupac_bw250_all_june.pdf feet

../output/nonparametric_rd_log_density_dupac_bw250_all_june.pdf: ../output/rd_summary_june_density_dupac_bw250_all.csv
	test -f "$@"

../output/rd_summary_june_density_far_bw500_multifamily.csv: ../../nonparametric_rd_density/code/nonparametric_rd_density.R ../input/june_parcels_with_ward_distances.csv | ../output
	RD_INPUT_PATH=../input/june_parcels_with_ward_distances.csv RD_SUMMARY_OUTPUT_PATH=$@ Rscript ../../nonparametric_rd_density/code/nonparametric_rd_density.R density_far TRUE 500 multifamily all ../output/nonparametric_rd_log_density_far_bw500_multifamily_june.pdf feet

../output/nonparametric_rd_log_density_far_bw500_multifamily_june.pdf: ../output/rd_summary_june_density_far_bw500_multifamily.csv
	test -f "$@"

../output/rd_summary_june_density_dupac_bw500_multifamily.csv: ../../nonparametric_rd_density/code/nonparametric_rd_density.R ../input/june_parcels_with_ward_distances.csv | ../output
	RD_INPUT_PATH=../input/june_parcels_with_ward_distances.csv RD_SUMMARY_OUTPUT_PATH=$@ Rscript ../../nonparametric_rd_density/code/nonparametric_rd_density.R density_dupac TRUE 500 multifamily all ../output/nonparametric_rd_log_density_dupac_bw500_multifamily_june.pdf feet

../output/nonparametric_rd_log_density_dupac_bw500_multifamily_june.pdf: ../output/rd_summary_june_density_dupac_bw500_multifamily.csv
	test -f "$@"

../output/rd_summary_dynamic_density_far_bw250_all.csv: ../../nonparametric_rd_density/code/nonparametric_rd_density.R ../output/parcels_with_ward_distances.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances.csv RD_SUMMARY_OUTPUT_PATH=$@ Rscript ../../nonparametric_rd_density/code/nonparametric_rd_density.R density_far TRUE 250 all all ../output/nonparametric_rd_log_density_far_bw250_all_dynamic_turnover.pdf feet

../output/nonparametric_rd_log_density_far_bw250_all_dynamic_turnover.pdf: ../output/rd_summary_dynamic_density_far_bw250_all.csv
	test -f "$@"

../output/rd_summary_dynamic_density_dupac_bw250_all.csv: ../../nonparametric_rd_density/code/nonparametric_rd_density.R ../output/parcels_with_ward_distances.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances.csv RD_SUMMARY_OUTPUT_PATH=$@ Rscript ../../nonparametric_rd_density/code/nonparametric_rd_density.R density_dupac TRUE 250 all all ../output/nonparametric_rd_log_density_dupac_bw250_all_dynamic_turnover.pdf feet

../output/nonparametric_rd_log_density_dupac_bw250_all_dynamic_turnover.pdf: ../output/rd_summary_dynamic_density_dupac_bw250_all.csv
	test -f "$@"

../output/rd_summary_dynamic_density_far_bw500_multifamily.csv: ../../nonparametric_rd_density/code/nonparametric_rd_density.R ../output/parcels_with_ward_distances.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances.csv RD_SUMMARY_OUTPUT_PATH=$@ Rscript ../../nonparametric_rd_density/code/nonparametric_rd_density.R density_far TRUE 500 multifamily all ../output/nonparametric_rd_log_density_far_bw500_multifamily_dynamic_turnover.pdf feet

../output/nonparametric_rd_log_density_far_bw500_multifamily_dynamic_turnover.pdf: ../output/rd_summary_dynamic_density_far_bw500_multifamily.csv
	test -f "$@"

../output/rd_summary_dynamic_density_dupac_bw500_multifamily.csv: ../../nonparametric_rd_density/code/nonparametric_rd_density.R ../output/parcels_with_ward_distances.csv | ../output
	RD_INPUT_PATH=../output/parcels_with_ward_distances.csv RD_SUMMARY_OUTPUT_PATH=$@ Rscript ../../nonparametric_rd_density/code/nonparametric_rd_density.R density_dupac TRUE 500 multifamily all ../output/nonparametric_rd_log_density_dupac_bw500_multifamily_dynamic_turnover.pdf feet

../output/nonparametric_rd_log_density_dupac_bw500_multifamily_dynamic_turnover.pdf: ../output/rd_summary_dynamic_density_dupac_bw500_multifamily.csv
	test -f "$@"

../output/fe_summary_june_bw250_all.csv: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../input/june_parcels_with_ward_distances.csv | ../output
	FE_INPUT_PATH=../input/june_parcels_with_ward_distances.csv FE_SUMMARY_OUTPUT_PATH=$@ CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 250 all zonegroup_segment_year_additive ../output/fe_table_bw250_all_zonegroup_segment_year_additive_june.tex $(FE_YVARS)

../output/fe_table_bw250_all_zonegroup_segment_year_additive_june.tex: ../output/fe_summary_june_bw250_all.csv
	test -f "$@"

../output/fe_summary_june_bw500_multifamily.csv: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../input/june_parcels_with_ward_distances.csv | ../output
	FE_INPUT_PATH=../input/june_parcels_with_ward_distances.csv FE_SUMMARY_OUTPUT_PATH=$@ CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_june.tex $(FE_YVARS)

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_june.tex: ../output/fe_summary_june_bw500_multifamily.csv
	test -f "$@"

../output/fe_summary_dynamic_bw250_all.csv: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances.csv FE_SUMMARY_OUTPUT_PATH=$@ CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 250 all zonegroup_segment_year_additive ../output/fe_table_bw250_all_zonegroup_segment_year_additive_dynamic_turnover.tex $(FE_YVARS)

../output/fe_table_bw250_all_zonegroup_segment_year_additive_dynamic_turnover.tex: ../output/fe_summary_dynamic_bw250_all.csv
	test -f "$@"

../output/fe_summary_dynamic_bw500_multifamily.csv: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../output/parcels_with_ward_distances.csv | ../output
	FE_INPUT_PATH=../output/parcels_with_ward_distances.csv FE_SUMMARY_OUTPUT_PATH=$@ CLUSTER_LEVEL=ward_pair Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R 500 multifamily zonegroup_segment_year_additive ../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_dynamic_turnover.tex $(FE_YVARS)

../output/fe_table_bw500_multifamily_zonegroup_segment_year_additive_dynamic_turnover.tex: ../output/fe_summary_dynamic_bw500_multifamily.csv
	test -f "$@"

../output/dynamic_turnover_assignment_summary.csv: compare_dynamic_turnover_month.R ../input/june_parcels_pre_scores.csv ../input/june_parcels_with_ward_distances.csv ../output/parcels_pre_scores.csv ../output/parcels_with_ward_distances.csv ../output/synthetic_month_choice_diagnostics.csv ../output/rd_summary_june_density_far_bw250_all.csv ../output/rd_summary_june_density_dupac_bw250_all.csv ../output/rd_summary_june_density_far_bw500_multifamily.csv ../output/rd_summary_june_density_dupac_bw500_multifamily.csv ../output/rd_summary_dynamic_density_far_bw250_all.csv ../output/rd_summary_dynamic_density_dupac_bw250_all.csv ../output/rd_summary_dynamic_density_far_bw500_multifamily.csv ../output/rd_summary_dynamic_density_dupac_bw500_multifamily.csv ../output/fe_summary_june_bw250_all.csv ../output/fe_summary_june_bw500_multifamily.csv ../output/fe_summary_dynamic_bw250_all.csv ../output/fe_summary_dynamic_bw500_multifamily.csv | ../output
	Rscript compare_dynamic_turnover_month.R

../output/dynamic_turnover_assignment_by_year.csv: ../output/dynamic_turnover_assignment_summary.csv
	test -f "$@"

../output/dynamic_turnover_main_density_comparison.csv: ../output/dynamic_turnover_assignment_summary.csv
	test -f "$@"

../output/dynamic_turnover_spotcheck_examples.csv: ../output/dynamic_turnover_assignment_summary.csv
	test -f "$@"

../input/geocoded_residential_data.gpkg: ../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"

../input/ward_controls.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	ln -sf "$<" "$@"

../input/block_group_geometry_2019.gpkg: ../../create_block_group_controls/output/block_group_geometry_2019.gpkg | ../input
	ln -sf "$<" "$@"

../input/zoning_data_clean.gpkg: ../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
	ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(SCORE_SPEC).csv | ../input
	ln -sf "$<" "$@"

../input/june_parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf "$<" "$@"

../input/june_parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/geocoded_residential_data.gpkg ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/block_group_controls.csv ../input/block_group_geometry_2019.gpkg ../input/zoning_data_clean.gpkg ../input/parcel_segment_ids.csv ../input/aldermen_uncertainty_scores.csv ../input/june_parcels_pre_scores.csv ../input/june_parcels_with_ward_distances.csv

include ../../generic.make
