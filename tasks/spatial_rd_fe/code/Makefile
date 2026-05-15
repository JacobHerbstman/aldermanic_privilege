SHELL := bash

BW_M := 100
FE_SPEC := zonegroup_segment_year_additive
FE_SPEC_EXACT := zone_segment_year_additive
MAIN_SAMPLES := multifamily all
YVARS := density_far density_dupac
GAP_SPLITS := below_median above_median
DONUTS_M := 5 10
PLACEBO_SHIFTS_M := -100 100

MAIN_OUTPUTS := $(foreach sample,$(MAIN_SAMPLES),$(foreach yvar,$(YVARS),../output/rd_fe_plot_log_$(yvar)_$(BW_M)m_$(sample)_$(FE_SPEC)_clust_ward_pair.pdf))
GAP_OUTPUTS := $(foreach yvar,$(YVARS),$(foreach gap,$(GAP_SPLITS),../output/rd_fe_plot_log_$(yvar)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_gap_$(gap).pdf))
DONUT_OUTPUTS := $(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_M),../output/rd_fe_plot_log_$(yvar)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_donut$(donut)m.pdf))
PLACEBO_OUTPUTS := $(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_M),../output/placebo_rd_fe_plot_log_$(yvar)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_shift$(shift)m.pdf))
DIAG_BASE_SLOPE_OUTPUTS := $(foreach yvar,$(YVARS),../output/rd_fe_diag_plot_log_$(yvar)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_slope.pdf)
DIAG_BASE_LEVEL_OUTPUTS := $(foreach yvar,$(YVARS),../output/rd_fe_diag_plot_log_$(yvar)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_level.pdf)
DIAG_ZONE_OUTPUTS := $(foreach yvar,$(YVARS),../output/rd_fe_diag_plot_log_$(yvar)_$(BW_M)m_multifamily_$(FE_SPEC_EXACT)_clust_ward_pair_slope.pdf)
DIAG_PLACEBO_OUTPUTS := $(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_M),../output/placebo_rd_fe_diag_plot_log_$(yvar)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_shift$(shift)m.pdf))
DIAG_SUMMARY_OUTPUTS := \
	../output/rd_fe_plot_style_comparison_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair.pdf \
	../output/rd_fe_distance_bin_summary_$(BW_M)m.csv \
	../output/rd_fe_distance_bin_summary_$(BW_M)m.tex \
	../output/rd_fe_slope_diagnostic_comparison_$(BW_M)m_multifamily_clust_ward_pair.csv \
	../output/rd_fe_slope_diagnostic_comparison_$(BW_M)m_multifamily_clust_ward_pair.tex \
	../output/rd_fe_pair_slope_summary_$(BW_M)m_multifamily.csv \
	../output/rd_fe_case_study_pairs_$(BW_M)m_multifamily.csv \
	../output/rd_fe_case_study_maps_$(BW_M)m_multifamily.pdf

all: $(MAIN_OUTPUTS) $(DONUT_OUTPUTS) $(PLACEBO_OUTPUTS) $(DIAG_BASE_SLOPE_OUTPUTS) $(DIAG_BASE_LEVEL_OUTPUTS) $(DIAG_ZONE_OUTPUTS) $(DIAG_PLACEBO_OUTPUTS) $(DIAG_SUMMARY_OUTPUTS)

define MAIN_RULE
../output/rd_fe_plot_log_$(1)_$(BW_M)m_$(2)_$(FE_SPEC)_clust_ward_pair.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_M) $(2) $(FE_SPEC) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
	rm -f "$$(@:.pdf=_fit.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach sample,$(MAIN_SAMPLES),$(eval $(call MAIN_RULE,$(yvar),$(sample)))))

define GAP_RULE
../output/rd_fe_plot_log_$(1)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_gap_$(2).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_M) multifamily $(FE_SPEC) $$@ slope $(2)
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
	rm -f "$$(@:.pdf=_fit.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach gap,$(GAP_SPLITS),$(eval $(call GAP_RULE,$(yvar),$(gap)))))

define DONUT_RULE
../output/rd_fe_plot_log_$(1)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_donut$(2)m.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	DONUT_M=$(2) CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_M) multifamily $(FE_SPEC) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
	rm -f "$$(@:.pdf=_fit.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_M),$(eval $(call DONUT_RULE,$(yvar),$(donut)))))

define PLACEBO_RULE
../output/placebo_rd_fe_plot_log_$(1)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_shift$(2)m.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	PLACEBO_SHIFT_M=$(2) CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_M) multifamily $(FE_SPEC) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
	rm -f "$$(@:.pdf=_fit.csv)"
endef
$(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_M),$(eval $(call PLACEBO_RULE,$(yvar),$(shift)))))

define DIAG_BASE_SLOPE_RULE
../output/rd_fe_diag_plot_log_$(1)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_slope.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_M) multifamily $(FE_SPEC) $$@ slope
endef
$(foreach yvar,$(YVARS),$(eval $(call DIAG_BASE_SLOPE_RULE,$(yvar))))

define DIAG_BASE_LEVEL_RULE
../output/rd_fe_diag_plot_log_$(1)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_level.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_M) multifamily $(FE_SPEC) $$@ level
endef
$(foreach yvar,$(YVARS),$(eval $(call DIAG_BASE_LEVEL_RULE,$(yvar))))

define DIAG_ZONE_RULE
../output/rd_fe_diag_plot_log_$(1)_$(BW_M)m_multifamily_$(FE_SPEC_EXACT)_clust_ward_pair_slope.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_M) multifamily $(FE_SPEC_EXACT) $$@ slope
endef
$(foreach yvar,$(YVARS),$(eval $(call DIAG_ZONE_RULE,$(yvar))))

define DIAG_PLACEBO_RULE
../output/placebo_rd_fe_diag_plot_log_$(1)_$(BW_M)m_multifamily_$(FE_SPEC)_clust_ward_pair_shift$(2)m.pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	PLACEBO_SHIFT_M=$(2) CLUSTER_LEVEL=ward_pair Rscript spatial_rd_fe.R $(1) TRUE $(BW_M) multifamily $(FE_SPEC) $$@ slope
endef
$(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_M),$(eval $(call DIAG_PLACEBO_RULE,$(yvar),$(shift)))))

$(DIAG_SUMMARY_OUTPUTS): summarize_density_slope_diagnostics.R ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_400m.gpkg ../input/major_streets.gpkg $(DIAG_BASE_SLOPE_OUTPUTS) $(DIAG_BASE_LEVEL_OUTPUTS) $(DIAG_ZONE_OUTPUTS) $(DIAG_PLACEBO_OUTPUTS) | ../output
	Rscript summarize_density_slope_diagnostics.R

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_geometry.gpkg: ../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_400m.gpkg: ../../border_segment_creation/output/boundary_segments_400m.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../prepare_amenities_data/output/major_streets.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_400m.gpkg ../input/major_streets.gpkg

include ../../shell_functions.make
include ../../generic.make
