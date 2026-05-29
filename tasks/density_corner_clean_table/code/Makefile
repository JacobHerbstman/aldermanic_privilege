BW_M ?= 152.4
BW_LABEL ?= 500ft
SAMPLES := all multifamily

all: $(foreach sample,$(SAMPLES),../output/fe_table_$(BW_LABEL)_$(sample)_corner_clean_compare.tex)

define TABLE_RULE
../output/fe_table_$(BW_LABEL)_$(1)_corner_clean_compare.tex: build_density_corner_clean_table.R ../temp/fe_summary_$(BW_LABEL)_$(1)_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../temp/fe_summary_$(BW_LABEL)_$(1)_zonegroup_segment_year_additive_clust_ward_pair_corner_clean.csv ../input/boundary_ambiguity_by_bw.csv | ../output
	Rscript $$< $(BW_M) $(1) $(BW_LABEL)
endef
$(foreach sample,$(SAMPLES),$(eval $(call TABLE_RULE,$(sample))))

define BASELINE_RULE
../temp/fe_summary_$(BW_LABEL)_$(1)_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv: ../../_lib/border_pair_fe_regression_runner.R ../input/parcels_with_ward_distances.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_RESULT_LOCATION=temp FE_RESULT_TAG=baseline Rscript $$< $(BW_M) $(1) zonegroup_segment_year_additive all ward_pair 'log(density_far)' 'log(density_dupac)'
endef
$(foreach sample,$(SAMPLES),$(eval $(call BASELINE_RULE,$(sample))))

define CORNER_CLEAN_RULE
../temp/fe_summary_$(BW_LABEL)_$(1)_zonegroup_segment_year_additive_clust_ward_pair_corner_clean.csv: ../../_lib/border_pair_fe_regression_runner.R ../input/parcels_with_ward_distances.csv ../input/parcel_other_pair_distance.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) DROP_AMBIGUOUS_WITHIN_BW=TRUE WRITE_TEX=FALSE FE_RESULT_LOCATION=temp FE_RESULT_TAG=corner_clean Rscript $$< $(BW_M) $(1) zonegroup_segment_year_additive all ward_pair 'log(density_far)' 'log(density_dupac)'
endef
$(foreach sample,$(SAMPLES),$(eval $(call CORNER_CLEAN_RULE,$(sample))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcel_other_pair_distance.csv: ../../border_pair_boundary_ambiguity/output/parcel_other_pair_distance.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_ambiguity_by_bw.csv: ../../border_pair_boundary_ambiguity/output/boundary_ambiguity_by_bw.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/parcel_other_pair_distance.csv ../input/boundary_ambiguity_by_bw.csv

include ../../generic.make
