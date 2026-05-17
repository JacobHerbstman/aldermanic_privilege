SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
BW_M ?= 152.4
BW_LABEL ?= 500ft
BINS_PER_SIDE ?= 5
SAMPLES := all multifamily
GAP_SPLITS := above_median below_median
YVARS := density_far density_dupac

all: $(foreach sample,$(SAMPLES),$(foreach yvar,$(YVARS),$(foreach gap_split,$(GAP_SPLITS),../output/nonparametric_rd_density_gap_split_log_$(yvar)_$(BW_LABEL)_$(sample)_$(gap_split).pdf)))

define GAP_SPLIT_RULE
../output/nonparametric_rd_density_gap_split_log_$(1)_$(BW_LABEL)_$(2)_$(3).pdf: nonparametric_rd_density_gap_split.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	DISTANCE_DISPLAY_UNIT=ft Rscript nonparametric_rd_density_gap_split.R $(1) $(BW_M) $(2) $(FE_SPEC) $(BINS_PER_SIDE) $(3) ../input/parcels_with_ward_distances.csv $$@
endef
$(foreach yvar,$(YVARS),$(foreach sample,$(SAMPLES),$(foreach gap_split,$(GAP_SPLITS),$(eval $(call GAP_SPLIT_RULE,$(yvar),$(sample),$(gap_split))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
