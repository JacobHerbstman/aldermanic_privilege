SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
BW_M ?= 152.4
BW_LABEL ?= 500ft
BINS_PER_SIDE ?= 5
SAMPLES := all multifamily
PLACEBO_SHIFT_M ?= 152.4
PLACEBO_SHIFT_LABEL ?= 500ft
YVARS := density_far density_dupac

all: \
	../output/nonparametric_rd_density_placebo_log_density_far_500ft_all_shift_neg500ft.pdf \
	../output/nonparametric_rd_density_placebo_log_density_far_500ft_all_shift_pos500ft.pdf \
	../output/nonparametric_rd_density_placebo_log_density_dupac_500ft_all_shift_neg500ft.pdf \
	../output/nonparametric_rd_density_placebo_log_density_dupac_500ft_all_shift_pos500ft.pdf \
	../output/nonparametric_rd_density_placebo_log_density_far_500ft_multifamily_shift_neg500ft.pdf \
	../output/nonparametric_rd_density_placebo_log_density_far_500ft_multifamily_shift_pos500ft.pdf \
	../output/nonparametric_rd_density_placebo_log_density_dupac_500ft_multifamily_shift_neg500ft.pdf \
	../output/nonparametric_rd_density_placebo_log_density_dupac_500ft_multifamily_shift_pos500ft.pdf

define PLACEBO_NEG_RULE
../output/nonparametric_rd_density_placebo_log_$(1)_$(BW_LABEL)_$(2)_shift_neg$(PLACEBO_SHIFT_LABEL).pdf: nonparametric_rd_density_placebo.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	DISTANCE_DISPLAY_UNIT=ft Rscript nonparametric_rd_density_placebo.R $(1) $(BW_M) $(2) $(FE_SPEC) $(BINS_PER_SIDE) -$(PLACEBO_SHIFT_M)
endef
$(foreach yvar,$(YVARS),$(foreach sample,$(SAMPLES),$(eval $(call PLACEBO_NEG_RULE,$(yvar),$(sample)))))

define PLACEBO_POS_RULE
../output/nonparametric_rd_density_placebo_log_$(1)_$(BW_LABEL)_$(2)_shift_pos$(PLACEBO_SHIFT_LABEL).pdf: nonparametric_rd_density_placebo.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	DISTANCE_DISPLAY_UNIT=ft Rscript nonparametric_rd_density_placebo.R $(1) $(BW_M) $(2) $(FE_SPEC) $(BINS_PER_SIDE) $(PLACEBO_SHIFT_M)
endef
$(foreach yvar,$(YVARS),$(foreach sample,$(SAMPLES),$(eval $(call PLACEBO_POS_RULE,$(yvar),$(sample)))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
