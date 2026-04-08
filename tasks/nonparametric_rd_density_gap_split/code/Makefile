SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
BW_FT := 500
BINS_PER_SIDE ?= 10
SAMPLE := multifamily
GAP_SPLITS := above_median below_median
YVARS := density_far density_dupac

all: $(foreach yvar,$(YVARS),$(foreach gap_split,$(GAP_SPLITS),../output/nonparametric_rd_density_gap_split_log_$(yvar)_bw$(BW_FT)_$(SAMPLE)_$(gap_split).pdf))

define GAP_SPLIT_RULE
../output/nonparametric_rd_density_gap_split_log_$(1)_bw$(BW_FT)_$(SAMPLE)_$(2).pdf: nonparametric_rd_density_gap_split.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_gap_split.R $(1) $(BW_FT) $(SAMPLE) $(FE_SPEC) $(BINS_PER_SIDE) $(2) $$@
endef
$(foreach yvar,$(YVARS),$(foreach gap_split,$(GAP_SPLITS),$(eval $(call GAP_SPLIT_RULE,$(yvar),$(gap_split)))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
