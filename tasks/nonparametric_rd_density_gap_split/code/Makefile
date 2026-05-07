SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
BW_M ?= 100
BW_FT := $(shell Rscript -e 'cat(round($(BW_M) / 0.3048))')
BINS_PER_SIDE ?= 5
SAMPLE ?= all
GAP_SPLITS := above_median below_median
YVARS := density_far density_dupac

all: $(foreach yvar,$(YVARS),$(foreach gap_split,$(GAP_SPLITS),../output/nonparametric_rd_density_gap_split_log_$(yvar)_$(BW_M)m_$(SAMPLE)_$(gap_split).pdf))

define GAP_SPLIT_RULE
../output/nonparametric_rd_density_gap_split_log_$(1)_$(BW_M)m_$(SAMPLE)_$(2).pdf: nonparametric_rd_density_gap_split.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_gap_split.R $(1) $(BW_FT) $(SAMPLE) $(FE_SPEC) $(BINS_PER_SIDE) $(2) ../input/parcels_with_ward_distances.csv $$@
endef
$(foreach yvar,$(YVARS),$(foreach gap_split,$(GAP_SPLITS),$(eval $(call GAP_SPLIT_RULE,$(yvar),$(gap_split)))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
