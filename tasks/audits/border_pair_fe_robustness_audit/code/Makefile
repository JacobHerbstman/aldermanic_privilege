SHELL := bash

MAIN_YVARS := 'log(density_far)' 'log(density_dupac)'
MAIN_BW_M ?= 152.4
MAIN_BW_LABEL ?= 500ft

all: \
	../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.tex \
	../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.tex

../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.tex: ../../../border_pair_FE_regressions/code/combine_density_main_table.R ../output/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.csv ../output/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.csv | ../output
	cd ../../../border_pair_FE_regressions/code && Rscript combine_density_main_table.R ../../audits/border_pair_fe_robustness_audit/output/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.csv ../../audits/border_pair_fe_robustness_audit/output/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.csv ../../audits/border_pair_fe_robustness_audit/output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.tex

../output/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.csv: ../../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	cd ../../../border_pair_FE_regressions/code && DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(MAIN_BW_LABEL) MIN_SEGMENT_LENGTH_FT=500 CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_INPUT_PATH=../../audits/border_pair_fe_robustness_audit/input/parcels_with_ward_distances.csv FE_SUMMARY_OUTPUT_PATH=../../audits/border_pair_fe_robustness_audit/output/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.csv Rscript border_pair_FE_regressions.R $(MAIN_BW_M) all zonegroup_segment_year_additive ../../audits/border_pair_fe_robustness_audit/output/fe_table_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.tex $(MAIN_YVARS)

../output/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.csv: ../../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	cd ../../../border_pair_FE_regressions/code && DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(MAIN_BW_LABEL) MIN_SEGMENT_LENGTH_FT=500 CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_INPUT_PATH=../../audits/border_pair_fe_robustness_audit/input/parcels_with_ward_distances.csv FE_SUMMARY_OUTPUT_PATH=../../audits/border_pair_fe_robustness_audit/output/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.csv Rscript border_pair_FE_regressions.R $(MAIN_BW_M) multifamily zonegroup_segment_year_additive ../../audits/border_pair_fe_robustness_audit/output/fe_table_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment500ft.tex $(MAIN_YVARS)

../output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.tex: ../../../border_pair_FE_regressions/code/combine_density_main_table.R ../output/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.csv ../output/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.csv | ../output
	cd ../../../border_pair_FE_regressions/code && Rscript combine_density_main_table.R ../../audits/border_pair_fe_robustness_audit/output/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.csv ../../audits/border_pair_fe_robustness_audit/output/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.csv ../../audits/border_pair_fe_robustness_audit/output/fe_table_$(MAIN_BW_LABEL)_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.tex

../output/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.csv: ../../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	cd ../../../border_pair_FE_regressions/code && DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(MAIN_BW_LABEL) MIN_SEGMENT_LENGTH_FT=1000 CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_INPUT_PATH=../../audits/border_pair_fe_robustness_audit/input/parcels_with_ward_distances.csv FE_SUMMARY_OUTPUT_PATH=../../audits/border_pair_fe_robustness_audit/output/fe_summary_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.csv Rscript border_pair_FE_regressions.R $(MAIN_BW_M) all zonegroup_segment_year_additive ../../audits/border_pair_fe_robustness_audit/output/fe_table_$(MAIN_BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.tex $(MAIN_YVARS)

../output/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.csv: ../../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	cd ../../../border_pair_FE_regressions/code && DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(MAIN_BW_LABEL) MIN_SEGMENT_LENGTH_FT=1000 CLUSTER_LEVEL=ward_pair WRITE_TEX=FALSE FE_INPUT_PATH=../../audits/border_pair_fe_robustness_audit/input/parcels_with_ward_distances.csv FE_SUMMARY_OUTPUT_PATH=../../audits/border_pair_fe_robustness_audit/output/fe_summary_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.csv Rscript border_pair_FE_regressions.R $(MAIN_BW_M) multifamily zonegroup_segment_year_additive ../../audits/border_pair_fe_robustness_audit/output/fe_table_$(MAIN_BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_minsegment1000ft.tex $(MAIN_YVARS)

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../../shell_functions.make
include ../../../generic.make
