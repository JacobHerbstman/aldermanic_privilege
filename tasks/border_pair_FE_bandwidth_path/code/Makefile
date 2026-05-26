SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
CLUSTER_LEVEL := ward_pair
BANDWIDTHS_M ?= 76.2 152.4 228.6 304.8
REFERENCE_BANDWIDTHS_M ?= 152.4
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
../temp/border_pair_fe_bandwidth_path_$(1)m_$(2)_main.csv: ../../_lib/border_pair_fe_regression_runner.R ../input/parcels_with_ward_distances.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft WRITE_TEX=FALSE FE_SUMMARY_OUTPUT_PATH=$$@ CLUSTER_LEVEL=$(CLUSTER_LEVEL) Rscript ../../_lib/border_pair_fe_regression_runner.R $(1) $(2) $(FE_SPEC) ../temp/ignore_$(1)m_$(2)_main.tex 'log(density_far)' 'log(density_dupac)'
endef
$(foreach bandwidth,$(BANDWIDTHS_M),$(foreach sample,$(SAMPLES),$(eval $(call MAIN_SUMMARY_RULE,$(bandwidth),$(sample)))))

define PRUNED_SUMMARY_RULE
../temp/border_pair_fe_bandwidth_path_$(1)m_$(2)_pruned.csv: ../../_lib/border_pair_fe_regression_runner.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft WRITE_TEX=FALSE PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CONFOUND_SEGMENT_FLAGS_PATH=../input/confounded_segment_flags.csv FE_SUMMARY_OUTPUT_PATH=$$@ CLUSTER_LEVEL=$(CLUSTER_LEVEL) Rscript ../../_lib/border_pair_fe_regression_runner.R $(1) $(2) $(FE_SPEC) ../temp/ignore_$(1)m_$(2)_pruned.tex 'log(density_far)' 'log(density_dupac)'
endef
$(foreach bandwidth,$(BANDWIDTHS_M),$(foreach sample,$(SAMPLES),$(eval $(call PRUNED_SUMMARY_RULE,$(bandwidth),$(sample)))))

../output/border_pair_fe_bandwidth_path.csv: plot_bandwidth_path.R \
$(foreach bandwidth,$(BANDWIDTHS_M),$(foreach sample,$(SAMPLES),../temp/border_pair_fe_bandwidth_path_$(bandwidth)m_$(sample)_main.csv ../temp/border_pair_fe_bandwidth_path_$(bandwidth)m_$(sample)_pruned.csv)) | ../output
	DISTANCE_DISPLAY_UNIT=ft Rscript plot_bandwidth_path.R ../temp ../output/border_pair_fe_bandwidth_path.csv ../output/border_pair_fe_bandwidth_path_log_density_far_multifamily_main.pdf ../output/border_pair_fe_bandwidth_path_log_density_far_multifamily_pruned.pdf ../output/border_pair_fe_bandwidth_path_log_density_far_all_main.pdf ../output/border_pair_fe_bandwidth_path_log_density_far_all_pruned.pdf ../output/border_pair_fe_bandwidth_path_log_density_dupac_multifamily_main.pdf ../output/border_pair_fe_bandwidth_path_log_density_dupac_multifamily_pruned.pdf ../output/border_pair_fe_bandwidth_path_log_density_dupac_all_main.pdf ../output/border_pair_fe_bandwidth_path_log_density_dupac_all_pruned.pdf "$(BANDWIDTHS_M)" "$(SAMPLES)" "$(PRUNE_SPECS)" "$(REFERENCE_BANDWIDTHS_M)"

../output/border_pair_fe_bandwidth_path_log_density_far_multifamily_main.pdf: ../output/border_pair_fe_bandwidth_path.csv
	@test -s "$@"

../output/border_pair_fe_bandwidth_path_log_density_far_multifamily_pruned.pdf: ../output/border_pair_fe_bandwidth_path.csv
	@test -s "$@"

../output/border_pair_fe_bandwidth_path_log_density_far_all_main.pdf: ../output/border_pair_fe_bandwidth_path.csv
	@test -s "$@"

../output/border_pair_fe_bandwidth_path_log_density_far_all_pruned.pdf: ../output/border_pair_fe_bandwidth_path.csv
	@test -s "$@"

../output/border_pair_fe_bandwidth_path_log_density_dupac_multifamily_main.pdf: ../output/border_pair_fe_bandwidth_path.csv
	@test -s "$@"

../output/border_pair_fe_bandwidth_path_log_density_dupac_multifamily_pruned.pdf: ../output/border_pair_fe_bandwidth_path.csv
	@test -s "$@"

../output/border_pair_fe_bandwidth_path_log_density_dupac_all_main.pdf: ../output/border_pair_fe_bandwidth_path.csv
	@test -s "$@"

../output/border_pair_fe_bandwidth_path_log_density_dupac_all_pruned.pdf: ../output/border_pair_fe_bandwidth_path.csv
	@test -s "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/confounded_segment_flags.csv: ../../border_confound_pruning/output/confounded_segment_flags.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv ../input/confounded_segment_flags.csv

include ../../generic.make
