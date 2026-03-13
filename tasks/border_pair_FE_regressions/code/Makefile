SHELL := bash
include ../../shell_functions.make

BWS_FT := 250 500 1000
SAMPLES := all multifamily
MAIN_FE_SPECS := segment_year zone_segment_year_additive zonegroup_segment_year_additive
ALT_FE_SPECS := pair_year_only pair_x_year zone_pair_year_additive zonegroup_pair_year_additive
MAIN_YVARS := 'log(density_far)' 'log(density_dupac)' 'log(unitscount)'
DONUTS_FT := 25 50

MAIN_OUTPUTS := $(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(MAIN_FE_SPECS),../output/fe_table_bw$(bw)_$(sample)_$(fe)_clust_segment.tex)))
MAIN_OUTPUTS_PRUNED := $(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(MAIN_FE_SPECS),../output/fe_table_bw$(bw)_$(sample)_$(fe)_clust_segment_pruned.tex)))
ROBUST_CLUSTER_OUTPUTS := $(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(MAIN_FE_SPECS),../output/fe_table_bw$(bw)_$(sample)_$(fe)_clust_ward_pair.tex)))
ALT_FE_OUTPUTS := $(foreach sample,$(SAMPLES),$(foreach fe,$(ALT_FE_SPECS),../output/fe_table_bw500_$(sample)_$(fe)_clust_segment.tex))
DONUT_OUTPUTS := $(foreach donut,$(DONUTS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(MAIN_FE_SPECS),../output/fe_table_bw500_$(sample)_$(fe)_clust_segment_donut$(donut).tex)))

all: $(MAIN_OUTPUTS)

define MAIN_RULE
../output/fe_table_bw$(1)_$(2)_$(3)_clust_segment.tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=segment Rscript border_pair_FE_regressions.R $(1) $(2) $(3) "$$@" $(MAIN_YVARS)
endef
$(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(MAIN_FE_SPECS),$(eval $(call MAIN_RULE,$(bw),$(sample),$(fe))))))

define MAIN_RULE_PRUNED
../output/fe_table_bw$(1)_$(2)_$(3)_clust_segment_pruned.tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CLUSTER_LEVEL=segment Rscript border_pair_FE_regressions.R $(1) $(2) $(3) "$$@" $(MAIN_YVARS)
endef
$(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(MAIN_FE_SPECS),$(eval $(call MAIN_RULE_PRUNED,$(bw),$(sample),$(fe))))))

define ROBUST_CLUSTER_RULE
../output/fe_table_bw$(1)_$(2)_$(3)_clust_ward_pair.tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions.R $(1) $(2) $(3) "$$@" $(MAIN_YVARS)
endef
$(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(MAIN_FE_SPECS),$(eval $(call ROBUST_CLUSTER_RULE,$(bw),$(sample),$(fe))))))

define ALT_FE_RULE
../output/fe_table_bw500_$(1)_$(2)_clust_segment.tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=segment Rscript border_pair_FE_regressions.R 500 $(1) $(2) "$$@" $(MAIN_YVARS)
endef
$(foreach sample,$(SAMPLES),$(foreach fe,$(ALT_FE_SPECS),$(eval $(call ALT_FE_RULE,$(sample),$(fe)))))

define DONUT_RULE
../output/fe_table_bw500_$(1)_$(2)_clust_segment_donut$(3).tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	DONUT_FT=$(3) CLUSTER_LEVEL=segment Rscript border_pair_FE_regressions.R 500 $(1) $(2) "$$@" $(MAIN_YVARS)
endef
$(foreach donut,$(DONUTS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(MAIN_FE_SPECS),$(eval $(call DONUT_RULE,$(sample),$(fe),$(donut))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv
