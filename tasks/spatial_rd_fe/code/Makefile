SHELL := bash
include ../../shell_functions.make

BW_FT := 500
FE_SPEC := zonegroup_segment_year_additive
FE_SPEC_EXACT := zone_segment_year_additive
MAIN_SAMPLES := multifamily all
YVARS := density_far density_dupac
GAP_SPLITS := below_median above_median
DONUTS_FT := 25 50
PLACEBO_SHIFTS_FT := -500 500

MAIN_OUTPUTS := $(foreach sample,$(MAIN_SAMPLES),$(foreach yvar,$(YVARS),../output/rd_fe_plot_log_$(yvar)_bw$(BW_FT)_$(sample)_$(FE_SPEC)_clust_ward_pair.pdf))
GAP_OUTPUTS := $(foreach yvar,$(YVARS),$(foreach gap,$(GAP_SPLITS),../output/rd_fe_plot_log_$(yvar)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_gap_$(gap).pdf))
DONUT_OUTPUTS := $(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),../output/rd_fe_plot_log_$(yvar)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_donut$(donut).pdf))
PLACEBO_OUTPUTS := $(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),../output/placebo_rd_fe_plot_log_$(yvar)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_shift$(shift).pdf))
DIAG_BASE_SLOPE_OUTPUTS := $(foreach yvar,$(YVARS),../output/rd_fe_diag_plot_log_$(yvar)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_slope.pdf)
DIAG_BASE_LEVEL_OUTPUTS := $(foreach yvar,$(YVARS),../output/rd_fe_diag_plot_log_$(yvar)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_level.pdf)
DIAG_ZONE_OUTPUTS := $(foreach yvar,$(YVARS),../output/rd_fe_diag_plot_log_$(yvar)_bw$(BW_FT)_multifamily_$(FE_SPEC_EXACT)_clust_ward_pair_slope.pdf)
DIAG_PLACEBO_OUTPUTS := $(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),../output/placebo_rd_fe_diag_plot_log_$(yvar)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_shift$(shift).pdf))
DIAG_SUMMARY_OUTPUTS := \
	../output/rd_fe_plot_style_comparison_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair.pdf \
	../output/rd_fe_distance_bin_summary_bw$(BW_FT).csv \
	../output/rd_fe_distance_bin_summary_bw$(BW_FT).tex \
	../output/rd_fe_slope_diagnostic_comparison_bw$(BW_FT)_multifamily_clust_ward_pair.csv \
	../output/rd_fe_slope_diagnostic_comparison_bw$(BW_FT)_multifamily_clust_ward_pair.tex \
	../output/rd_fe_pair_slope_summary_bw$(BW_FT)_multifamily.csv \
	../output/rd_fe_case_study_pairs_bw$(BW_FT)_multifamily.csv \
	../output/rd_fe_case_study_maps_bw$(BW_FT)_multifamily.pdf

all: $(MAIN_OUTPUTS) $(GAP_OUTPUTS) $(DONUT_OUTPUTS) $(PLACEBO_OUTPUTS) $(DIAG_BASE_SLOPE_OUTPUTS) $(DIAG_BASE_LEVEL_OUTPUTS) $(DIAG_ZONE_OUTPUTS) $(DIAG_PLACEBO_OUTPUTS) $(DIAG_SUMMARY_OUTPUTS)

define MAIN_RULE
../output/rd_fe_plot_log_$(1)_bw$(BW_FT)_$(2)_$(FE_SPEC)_clust_ward_pair.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) $(2) $(FE_SPEC) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
	rm -f "$$(@:.pdf=_fit.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach sample,$(MAIN_SAMPLES),$(eval $(call MAIN_RULE,$(yvar),$(sample)))))

define GAP_RULE
../output/rd_fe_plot_log_$(1)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_gap_$(2).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) multifamily $(FE_SPEC) $$@ slope $(2)
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
	rm -f "$$(@:.pdf=_fit.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach gap,$(GAP_SPLITS),$(eval $(call GAP_RULE,$(yvar),$(gap)))))

define DONUT_RULE
../output/rd_fe_plot_log_$(1)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_donut$(2).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	DONUT_FT=$(2) CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) multifamily $(FE_SPEC) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
	rm -f "$$(@:.pdf=_fit.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),$(eval $(call DONUT_RULE,$(yvar),$(donut)))))

define PLACEBO_RULE
../output/placebo_rd_fe_plot_log_$(1)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_shift$(2).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	PLACEBO_SHIFT_FT=$(2) CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) multifamily $(FE_SPEC) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
	rm -f "$$(@:.pdf=_fit.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),$(eval $(call PLACEBO_RULE,$(yvar),$(shift)))))

define DIAG_BASE_SLOPE_RULE
../output/rd_fe_diag_plot_log_$(1)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_slope.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) multifamily $(FE_SPEC) $$@ slope
endef
$(foreach yvar,$(YVARS),$(eval $(call DIAG_BASE_SLOPE_RULE,$(yvar))))

define DIAG_BASE_LEVEL_RULE
../output/rd_fe_diag_plot_log_$(1)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_level.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) multifamily $(FE_SPEC) $$@ level
endef
$(foreach yvar,$(YVARS),$(eval $(call DIAG_BASE_LEVEL_RULE,$(yvar))))

define DIAG_ZONE_RULE
../output/rd_fe_diag_plot_log_$(1)_bw$(BW_FT)_multifamily_$(FE_SPEC_EXACT)_clust_ward_pair_slope.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) multifamily $(FE_SPEC_EXACT) $$@ slope
endef
$(foreach yvar,$(YVARS),$(eval $(call DIAG_ZONE_RULE,$(yvar))))

define DIAG_PLACEBO_RULE
../output/placebo_rd_fe_diag_plot_log_$(1)_bw$(BW_FT)_multifamily_$(FE_SPEC)_clust_ward_pair_shift$(2).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	PLACEBO_SHIFT_FT=$(2) CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_FT) multifamily $(FE_SPEC) $$@ slope
endef
$(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),$(eval $(call DIAG_PLACEBO_RULE,$(yvar),$(shift)))))

$(DIAG_SUMMARY_OUTPUTS) &: summarize_density_slope_diagnostics.R ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_1320ft.gpkg ../input/major_streets.gpkg $(DIAG_BASE_SLOPE_OUTPUTS) $(DIAG_BASE_LEVEL_OUTPUTS) $(DIAG_ZONE_OUTPUTS) $(DIAG_PLACEBO_OUTPUTS) | ../output
	Rscript summarize_density_slope_diagnostics.R

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/parcels_with_geometry.gpkg: ../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../download_amenities_data/output/major_streets.gpkg | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_1320ft.gpkg ../input/major_streets.gpkg

include ../../generic.make
