BW_M ?= 152.4
BW_LABEL ?= 500ft
FE_SPEC := zonegroup_segment_year_additive
CLUSTER_LEVEL := ward_pair

all: \
	../output/spatial_rd_multifamily_threshold_summary.csv \
	../output/spatial_rd_multifamily_threshold_table.tex \
	../output/spatial_rd_multifamily_threshold_estimates.pdf

../output/spatial_rd_multifamily_threshold_summary.csv: combine_threshold_results.R \
	../temp/fe_summary_500ft_minunits2_zonegroup_segment_year_additive_clust_ward_pair_main.csv \
	../temp/fe_summary_500ft_minunits3_zonegroup_segment_year_additive_clust_ward_pair_main.csv \
	../temp/fe_summary_500ft_minunits5_zonegroup_segment_year_additive_clust_ward_pair_main.csv \
	../temp/fe_summary_500ft_minunits2_nopd_zonegroup_segment_year_additive_clust_ward_pair_main.csv \
	../temp/fe_summary_500ft_minunits3_nopd_zonegroup_segment_year_additive_clust_ward_pair_main.csv \
	../temp/fe_summary_500ft_minunits5_nopd_zonegroup_segment_year_additive_clust_ward_pair_main.csv \
	../temp/fe_summary_500ft_minunits2_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv \
	../temp/fe_summary_500ft_minunits3_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv \
	../temp/fe_summary_500ft_minunits5_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv \
	../temp/fe_summary_500ft_minunits2_nopd_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv \
	../temp/fe_summary_500ft_minunits3_nopd_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv \
	../temp/fe_summary_500ft_minunits5_nopd_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv | ../output
	Rscript $< "2 3 5" "main pruned" "allzoning nopd"

../output/spatial_rd_multifamily_threshold_table.tex: ../output/spatial_rd_multifamily_threshold_summary.csv
	@test -s "$@"

../output/spatial_rd_multifamily_threshold_estimates.pdf: ../output/spatial_rd_multifamily_threshold_summary.csv
	@test -s "$@"

../temp/fe_summary_500ft_minunits2_zonegroup_segment_year_additive_clust_ward_pair_main.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits2.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits2.csv FE_SUMMARY_OUTPUT_PATH=$@ Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) all $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_500ft_minunits3_zonegroup_segment_year_additive_clust_ward_pair_main.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits3.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits3.csv FE_SUMMARY_OUTPUT_PATH=$@ Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) all $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_500ft_minunits5_zonegroup_segment_year_additive_clust_ward_pair_main.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits5.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits5.csv FE_SUMMARY_OUTPUT_PATH=$@ Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) all $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_500ft_minunits2_nopd_zonegroup_segment_year_additive_clust_ward_pair_main.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits2_nopd.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits2_nopd.csv FE_SUMMARY_OUTPUT_PATH=$@ Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) all $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_500ft_minunits3_nopd_zonegroup_segment_year_additive_clust_ward_pair_main.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits3_nopd.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits3_nopd.csv FE_SUMMARY_OUTPUT_PATH=$@ Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) all $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_500ft_minunits5_nopd_zonegroup_segment_year_additive_clust_ward_pair_main.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits5_nopd.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits5_nopd.csv FE_SUMMARY_OUTPUT_PATH=$@ Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) all $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_500ft_minunits2_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits2.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits2.csv FE_SUMMARY_OUTPUT_PATH=$@ CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) pruned $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_500ft_minunits3_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits3.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits3.csv FE_SUMMARY_OUTPUT_PATH=$@ CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) pruned $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_500ft_minunits5_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits5.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits5.csv FE_SUMMARY_OUTPUT_PATH=$@ CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) pruned $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_500ft_minunits2_nopd_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits2_nopd.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits2_nopd.csv FE_SUMMARY_OUTPUT_PATH=$@ CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) pruned $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_500ft_minunits3_nopd_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits3_nopd.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits3_nopd.csv FE_SUMMARY_OUTPUT_PATH=$@ CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) pruned $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_500ft_minunits5_nopd_zonegroup_segment_year_additive_clust_ward_pair_pruned.csv: ../../../_lib/border_pair_fe_regression_runner.R ../../../_lib/border_pair_helpers.R ../temp/parcels_with_ward_distances_minunits5_nopd.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_INPUT_PATH=../temp/parcels_with_ward_distances_minunits5_nopd.csv FE_SUMMARY_OUTPUT_PATH=$@ CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv Rscript ../../../_lib/border_pair_fe_regression_runner.R $(BW_M) multifamily $(FE_SPEC) pruned $(CLUSTER_LEVEL) 'log(density_far)' 'log(density_dupac)'

../temp/parcels_with_ward_distances_minunits2.csv: build_threshold_input.R ../input/parcels_with_ward_distances.csv | ../temp
	Rscript $< 2 allzoning

../temp/parcels_with_ward_distances_minunits3.csv: build_threshold_input.R ../input/parcels_with_ward_distances.csv | ../temp
	Rscript $< 3 allzoning

../temp/parcels_with_ward_distances_minunits5.csv: build_threshold_input.R ../input/parcels_with_ward_distances.csv | ../temp
	Rscript $< 5 allzoning

../temp/parcels_with_ward_distances_minunits2_nopd.csv: build_threshold_input.R ../input/parcels_with_ward_distances.csv | ../temp
	Rscript $< 2 nopd

../temp/parcels_with_ward_distances_minunits3_nopd.csv: build_threshold_input.R ../input/parcels_with_ward_distances.csv | ../temp
	Rscript $< 3 nopd

../temp/parcels_with_ward_distances_minunits5_nopd.csv: build_threshold_input.R ../input/parcels_with_ward_distances.csv | ../temp
	Rscript $< 5 nopd

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/confounded_segment_flags.csv: ../../../border_confound_pruning/output/confounded_segment_flags.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/parcels_with_ward_distances.csv \
	../input/confounded_pair_era_flags.csv \
	../input/confounded_segment_flags.csv

include ../../../generic.make
