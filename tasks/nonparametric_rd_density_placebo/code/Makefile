SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
BW_FT := 500
BINS_PER_SIDE ?= 5
SAMPLE := multifamily
PLACEBO_SHIFTS_FT := -500 500
YVARS := density_far density_dupac

all: $(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),../output/nonparametric_rd_density_placebo_log_$(yvar)_bw$(BW_FT)_$(SAMPLE)_shift$(shift).pdf))

define PLACEBO_RULE
../output/nonparametric_rd_density_placebo_log_$(1)_bw$(BW_FT)_$(SAMPLE)_shift$(2).pdf: nonparametric_rd_density_placebo.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_placebo.R $(1) $(BW_FT) $(SAMPLE) $(FE_SPEC) $(BINS_PER_SIDE) $(2) $$@
endef
$(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),$(eval $(call PLACEBO_RULE,$(yvar),$(shift)))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
