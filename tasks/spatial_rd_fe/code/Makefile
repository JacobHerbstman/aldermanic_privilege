SHELL := bash
include ../../shell_functions.make

BANDWIDTHS := 250 500 1000
MAIN_FE_SPECS := segment_year zone_segment_year_additive
ALT_FE_SPECS := pair_year zone_pair_year_additive pair_x_year
YVARS := density_far density_dupac unitscount
LOG_STATUS := TRUE
SAMPLES := all multifamily
GAP_SPLITS := above_median below_median
ROBUST_YVARS := density_far density_dupac
DONUTS_FT := 25 50
PLACEBO_SHIFTS_FT := -500 -250 250 500

LOG_PREFIX_TRUE := log_
LOG_PREFIX_FALSE :=

ALL_OUTPUTS := $(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_clust_segment.pdf)))))

all: $(ALL_OUTPUTS)

PRUNED_OUTPUTS := $(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_clust_segment_pruned.pdf)))))

pruned: $(PRUNED_OUTPUTS)

ROBUST_CLUSTER_OUTPUTS := $(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_clust_ward_pair.pdf)))))

ALT_FE_OUTPUTS := $(foreach yvar,$(YVARS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(ALT_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw500_$(sample)_$(fe)_clust_segment.pdf))))

DONUT_OUTPUTS := $(foreach yvar,$(ROBUST_YVARS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(foreach donut,$(DONUTS_FT),\
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw500_$(sample)_$(fe)_clust_segment_donut$(donut).pdf)))))

PLACEBO_OUTPUTS := $(foreach yvar,$(ROBUST_YVARS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(foreach shift,$(PLACEBO_SHIFTS_FT),\
../output/placebo_rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw500_$(sample)_$(fe)_clust_segment_shift$(shift).pdf)))))

robust-cluster: $(ROBUST_CLUSTER_OUTPUTS)
alt-fe: $(ALT_FE_OUTPUTS)
donut: $(DONUT_OUTPUTS)
placebo: $(PLACEBO_OUTPUTS)

GAP_OUTPUTS := $(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(foreach gap,$(GAP_SPLITS),\
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_clust_segment_gap_$(gap).pdf))))))

gap-split: $(GAP_OUTPUTS)

segment-robustness: robust-cluster

define GEN_RULE
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_clust_segment.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=segment Rscript spatial_rd_fe.R $(yvar) $(log_stat) $(bw) $(sample) $(fe) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef

$(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(eval $(GEN_RULE)))))))

define GEN_RULE_PRUNED
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_clust_segment_pruned.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv CLUSTER_LEVEL=segment Rscript spatial_rd_fe.R $(yvar) $(log_stat) $(bw) $(sample) $(fe) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef

$(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(eval $(GEN_RULE_PRUNED)))))))

define GEN_GAP_RULE
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_clust_segment_gap_$(gap).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=segment Rscript spatial_rd_fe.R $(yvar) $(log_stat) $(bw) $(sample) $(fe) $$@ slope $(gap)
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef

$(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(foreach gap,$(GAP_SPLITS),\
$(eval $(GEN_GAP_RULE))))))))

define GEN_ROBUST_CLUSTER_RULE
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(sample)_$(fe)_clust_ward_pair.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(yvar) $(log_stat) $(bw) $(sample) $(fe) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef

$(foreach yvar,$(YVARS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(eval $(GEN_ROBUST_CLUSTER_RULE)))))))

define GEN_ALT_FE_RULE
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw500_$(sample)_$(fe)_clust_segment.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=segment Rscript spatial_rd_fe.R $(yvar) $(log_stat) 500 $(sample) $(fe) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef

$(foreach yvar,$(YVARS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(ALT_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(eval $(GEN_ALT_FE_RULE))))))

define GEN_DONUT_RULE
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw500_$(sample)_$(fe)_clust_segment_donut$(donut).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	DONUT_FT=$(donut) CLUSTER_LEVEL=segment Rscript spatial_rd_fe.R $(yvar) $(log_stat) 500 $(sample) $(fe) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef

$(foreach yvar,$(ROBUST_YVARS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(foreach donut,$(DONUTS_FT),\
$(eval $(GEN_DONUT_RULE)))))))

define GEN_PLACEBO_RULE
../output/placebo_rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw500_$(sample)_$(fe)_clust_segment_shift$(shift).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	PLACEBO_SHIFT_FT=$(shift) CLUSTER_LEVEL=segment Rscript spatial_rd_fe.R $(yvar) $(log_stat) 500 $(sample) $(fe) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef

$(foreach yvar,$(ROBUST_YVARS),\
$(foreach sample,$(SAMPLES),\
$(foreach fe,$(MAIN_FE_SPECS),\
$(foreach log_stat,$(LOG_STATUS),\
$(foreach shift,$(PLACEBO_SHIFTS_FT),\
$(eval $(GEN_PLACEBO_RULE)))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv
