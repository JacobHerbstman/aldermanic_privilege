SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
BW_FT := 500
BINS_PER_SIDE ?= 10
SAMPLE := multifamily
DONUTS_FT := 25 50
YVARS := density_far density_dupac

all: $(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),../output/nonparametric_rd_density_donut_log_$(yvar)_bw$(BW_FT)_$(SAMPLE)_donut$(donut).pdf))

define DONUT_RULE
../output/nonparametric_rd_density_donut_log_$(1)_bw$(BW_FT)_$(SAMPLE)_donut$(2).pdf: nonparametric_rd_density_donut.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_donut.R $(1) $(BW_FT) $(SAMPLE) $(FE_SPEC) $(BINS_PER_SIDE) $(2) $$@
endef
$(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),$(eval $(call DONUT_RULE,$(yvar),$(donut)))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
