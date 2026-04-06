SHELL := bash
include ../../shell_functions.make

MAIN_YVARS := 'log(density_far)' 'log(density_dupac)'
BWS := 250 500 1000
SAMPLES := all multifamily
FE_SPECS := zonegroup_segment_year_additive zonegroup_pair_year_additive segment_year

TABLES := $(foreach bw,$(BWS),$(foreach sample,$(SAMPLES),$(foreach fe,$(FE_SPECS),../output/fe_table_mcculloch_bw$(bw)_$(sample)_$(fe)_clust_ward_pair.tex)))

all: $(TABLES)

define TABLE_template
../output/fe_table_mcculloch_bw$(1)_$(2)_$(3)_clust_ward_pair.tex: border_pair_FE_regressions_mcculloch.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript border_pair_FE_regressions_mcculloch.R $(1) $(2) $(3) "$$@" $(MAIN_YVARS)
endef

$(foreach bw,$(BWS),$(foreach sample,$(SAMPLES),$(foreach fe,$(FE_SPECS),$(eval $(call TABLE_template,$(bw),$(sample),$(fe))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
