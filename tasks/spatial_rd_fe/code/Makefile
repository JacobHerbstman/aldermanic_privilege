SHELL := bash
include ../../shell_functions.make

BW_FT := 500
FE_SPEC := zonegroup_segment_year_additive
MAIN_SAMPLES := multifamily all
YVARS := density_far density_dupac
GAP_SPLITS := below_median above_median
DONUTS_FT := 25 50
PLACEBO_SHIFTS_FT := -500 500

MAIN_OUTPUTS := $(foreach sample,$(MAIN_SAMPLES),$(foreach yvar,$(YVARS),../output/rd_fe_plot_log_$(yvar)_bw$(BW_FT)_$(sample)_$(FE_SPEC)_clust_ward_pair.pdf))
GAP_OUTPUTS := $(foreach yvar,$(YVARS),$(foreach gap,$(GAP_SPLITS),../output/rd_fe_plot_log_$(yvar)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_gap_$(gap).pdf))
DONUT_OUTPUTS := $(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),../output/rd_fe_plot_log_$(yvar)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_donut$(donut).pdf))
PLACEBO_OUTPUTS := $(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),../output/placebo_rd_fe_plot_log_$(yvar)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_shift$(shift).pdf))

all: $(MAIN_OUTPUTS) $(GAP_OUTPUTS) $(DONUT_OUTPUTS) $(PLACEBO_OUTPUTS)

define MAIN_RULE
../output/rd_fe_plot_log_$(1)_bw$(BW_FT)_$(2)_$(FE_SPEC)_clust_ward_pair.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) $(2) $(FE_SPEC) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach sample,$(MAIN_SAMPLES),$(eval $(call MAIN_RULE,$(yvar),$(sample)))))

define GAP_RULE
../output/rd_fe_plot_log_$(1)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_gap_$(2).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) multifamily $(FE_SPEC) $$@ slope $(2)
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach gap,$(GAP_SPLITS),$(eval $(call GAP_RULE,$(yvar),$(gap)))))

define DONUT_RULE
../output/rd_fe_plot_log_$(1)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_donut$(2).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	DONUT_FT=$(2) CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) multifamily $(FE_SPEC) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),$(eval $(call DONUT_RULE,$(yvar),$(donut)))))

define PLACEBO_RULE
../output/placebo_rd_fe_plot_log_$(1)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_shift$(2).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	PLACEBO_SHIFT_FT=$(2) CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) multifamily $(FE_SPEC) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),$(eval $(call PLACEBO_RULE,$(yvar),$(shift)))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
