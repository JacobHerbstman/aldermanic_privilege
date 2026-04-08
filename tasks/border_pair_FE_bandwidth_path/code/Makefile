SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
CLUSTER_LEVEL := ward_pair
BANDWIDTHS := 100 150 200 250 300 350 400 450 500 550 600 650 700 750 800 850 900 950 1000
SAMPLES := multifamily all
PRUNE_SPECS := main pruned

all: \
	../output/border_pair_fe_bandwidth_path.csv \
	../output/border_pair_fe_bandwidth_path_log_density_far_multifamily_main.pdf \
	../output/border_pair_fe_bandwidth_path_log_density_far_multifamily_pruned.pdf \
	../output/border_pair_fe_bandwidth_path_log_density_far_all_main.pdf \
	../output/border_pair_fe_bandwidth_path_log_density_far_all_pruned.pdf \
	../output/border_pair_fe_bandwidth_path_log_density_dupac_multifamily_main.pdf \
	../output/border_pair_fe_bandwidth_path_log_density_dupac_multifamily_pruned.pdf \
	../output/border_pair_fe_bandwidth_path_log_density_dupac_all_main.pdf \
	../output/border_pair_fe_bandwidth_path_log_density_dupac_all_pruned.pdf

define MAIN_SUMMARY_RULE
../temp/border_pair_fe_bandwidth_path_bw$(1)_$(2)_main.csv: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../temp
	WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH=$$@ CLUSTER_LEVEL=$(CLUSTER_LEVEL) Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R $(1) $(2) $(FE_SPEC) ../temp/ignore_bw$(1)_$(2)_main.tex 'log(density_far)' 'log(density_dupac)'
endef
$(foreach bandwidth,$(BANDWIDTHS),$(foreach sample,$(SAMPLES),$(eval $(call MAIN_SUMMARY_RULE,$(bandwidth),$(sample)))))

define PRUNED_SUMMARY_RULE
../temp/border_pair_fe_bandwidth_path_bw$(1)_$(2)_pruned.csv: ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv | ../temp
	WRITE_TEX=FALSE PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv FE_SUMMARY_OUTPUT_PATH=$$@ CLUSTER_LEVEL=$(CLUSTER_LEVEL) Rscript ../../border_pair_FE_regressions/code/border_pair_FE_regressions.R $(1) $(2) $(FE_SPEC) ../temp/ignore_bw$(1)_$(2)_pruned.tex 'log(density_far)' 'log(density_dupac)'
endef
$(foreach bandwidth,$(BANDWIDTHS),$(foreach sample,$(SAMPLES),$(eval $(call PRUNED_SUMMARY_RULE,$(bandwidth),$(sample)))))

../output/border_pair_fe_bandwidth_path.csv: plot_bandwidth_path.R \
$(foreach bandwidth,$(BANDWIDTHS),$(foreach sample,$(SAMPLES),../temp/border_pair_fe_bandwidth_path_bw$(bandwidth)_$(sample)_main.csv ../temp/border_pair_fe_bandwidth_path_bw$(bandwidth)_$(sample)_pruned.csv)) | ../output
	Rscript plot_bandwidth_path.R ../temp ../output/border_pair_fe_bandwidth_path.csv ../output/border_pair_fe_bandwidth_path_log_density_far_multifamily_main.pdf ../output/border_pair_fe_bandwidth_path_log_density_far_multifamily_pruned.pdf ../output/border_pair_fe_bandwidth_path_log_density_far_all_main.pdf ../output/border_pair_fe_bandwidth_path_log_density_far_all_pruned.pdf ../output/border_pair_fe_bandwidth_path_log_density_dupac_multifamily_main.pdf ../output/border_pair_fe_bandwidth_path_log_density_dupac_multifamily_pruned.pdf ../output/border_pair_fe_bandwidth_path_log_density_dupac_all_main.pdf ../output/border_pair_fe_bandwidth_path_log_density_dupac_all_pruned.pdf "$(BANDWIDTHS)" "$(SAMPLES)" "$(PRUNE_SPECS)"

../output/border_pair_fe_bandwidth_path_log_density_far_multifamily_main.pdf: ../output/border_pair_fe_bandwidth_path.csv
	test -f "$@"

../output/border_pair_fe_bandwidth_path_log_density_far_multifamily_pruned.pdf: ../output/border_pair_fe_bandwidth_path.csv
	test -f "$@"

../output/border_pair_fe_bandwidth_path_log_density_far_all_main.pdf: ../output/border_pair_fe_bandwidth_path.csv
	test -f "$@"

../output/border_pair_fe_bandwidth_path_log_density_far_all_pruned.pdf: ../output/border_pair_fe_bandwidth_path.csv
	test -f "$@"

../output/border_pair_fe_bandwidth_path_log_density_dupac_multifamily_main.pdf: ../output/border_pair_fe_bandwidth_path.csv
	test -f "$@"

../output/border_pair_fe_bandwidth_path_log_density_dupac_multifamily_pruned.pdf: ../output/border_pair_fe_bandwidth_path.csv
	test -f "$@"

../output/border_pair_fe_bandwidth_path_log_density_dupac_all_main.pdf: ../output/border_pair_fe_bandwidth_path.csv
	test -f "$@"

../output/border_pair_fe_bandwidth_path_log_density_dupac_all_pruned.pdf: ../output/border_pair_fe_bandwidth_path.csv
	test -f "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv

include ../../generic.make
