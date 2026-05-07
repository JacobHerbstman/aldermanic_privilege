SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
BW_M ?= 100
BW_FT := $(shell Rscript -e 'cat(round($(BW_M) / 0.3048))')
BINS_PER_SIDE ?= 5
SAMPLE ?= all
PLACEBO_SHIFT_M ?= 100
PLACEBO_SHIFT_FT := $(shell Rscript -e 'cat(round($(PLACEBO_SHIFT_M) / 0.3048))')
YVARS := density_far density_dupac

all: $(foreach yvar,$(YVARS),../output/nonparametric_rd_density_placebo_log_$(yvar)_$(BW_M)m_$(SAMPLE)_shift_neg$(PLACEBO_SHIFT_M)m.pdf ../output/nonparametric_rd_density_placebo_log_$(yvar)_$(BW_M)m_$(SAMPLE)_shift_pos$(PLACEBO_SHIFT_M)m.pdf)

define PLACEBO_NEG_RULE
../output/nonparametric_rd_density_placebo_log_$(1)_$(BW_M)m_$(SAMPLE)_shift_neg$(PLACEBO_SHIFT_M)m.pdf: nonparametric_rd_density_placebo.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_placebo.R $(1) $(BW_FT) $(SAMPLE) $(FE_SPEC) $(BINS_PER_SIDE) -$(PLACEBO_SHIFT_FT) ../input/parcels_with_ward_distances.csv $$@ meters
endef
$(foreach yvar,$(YVARS),$(eval $(call PLACEBO_NEG_RULE,$(yvar))))

define PLACEBO_POS_RULE
../output/nonparametric_rd_density_placebo_log_$(1)_$(BW_M)m_$(SAMPLE)_shift_pos$(PLACEBO_SHIFT_M)m.pdf: nonparametric_rd_density_placebo.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_placebo.R $(1) $(BW_FT) $(SAMPLE) $(FE_SPEC) $(BINS_PER_SIDE) $(PLACEBO_SHIFT_FT) ../input/parcels_with_ward_distances.csv $$@ meters
endef
$(foreach yvar,$(YVARS),$(eval $(call PLACEBO_POS_RULE,$(yvar))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
