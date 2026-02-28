SHELL := bash
include ../../shell_functions.make

BANDWIDTHS := 250 500 1000
FE_SPECS := pair_year zone_pair_year_additive
YVARS := density_far density_dupac unitscount
LOG_STATUS := TRUE
SAMPLES := all multifamily
GAP_SPLITS := above_median below_median

LOG_PREFIX_TRUE := log_
LOG_PREFIX_FALSE :=

ALL_OUTPUTS := $(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe).pdf)))))

all: $(ALL_OUTPUTS)

PRUNED_OUTPUTS := $(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_pruned.pdf)))))

pruned: $(PRUNED_OUTPUTS)

GAP_OUTPUTS := $(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(foreach gap,$(GAP_SPLITS),\
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_gap_$(gap).pdf))))))

gap-split: $(GAP_OUTPUTS)

define GEN_RULE
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript spatial_rd_fe.R $(yvar) $(log_stat) $(bw) $(sample) $(fe) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef

$(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(eval $(GEN_RULE)))))))

define GEN_RULE_PRUNED
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_pruned.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv Rscript spatial_rd_fe.R $(yvar) $(log_stat) $(bw) $(sample) $(fe) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef

$(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(eval $(GEN_RULE_PRUNED)))))))

define GEN_GAP_RULE
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_gap_$(gap).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript spatial_rd_fe.R $(yvar) $(log_stat) $(bw) $(sample) $(fe) $$@ slope $(gap)
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef

$(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(foreach gap,$(GAP_SPLITS),\
$(eval $(GEN_GAP_RULE))))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv
