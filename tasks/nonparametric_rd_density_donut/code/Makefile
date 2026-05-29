FE_SPEC := zonegroup_segment_year_additive
BW_M ?= 152.4
BW_LABEL ?= 500ft
BINS_PER_SIDE ?= 5
SAMPLES := all multifamily
DONUT_1_M ?= 7.62
DONUT_1_LABEL ?= 25ft
DONUT_2_M ?= 15.24
DONUT_2_LABEL ?= 50ft
YVARS := density_far density_dupac

all: $(foreach sample,$(SAMPLES),$(foreach yvar,$(YVARS),../output/nonparametric_rd_density_donut_log_$(yvar)_$(BW_LABEL)_$(sample)_donut$(DONUT_1_LABEL).pdf ../output/nonparametric_rd_density_donut_log_$(yvar)_$(BW_LABEL)_$(sample)_donut$(DONUT_2_LABEL).pdf))

define DONUT_RULE_1
../output/nonparametric_rd_density_donut_log_$(1)_$(BW_LABEL)_$(2)_donut$(DONUT_1_LABEL).pdf: nonparametric_rd_density_donut.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_donut.R $(1) $(BW_M) $(2) $(FE_SPEC) $(BINS_PER_SIDE) $(DONUT_1_M)
endef
$(foreach yvar,$(YVARS),$(foreach sample,$(SAMPLES),$(eval $(call DONUT_RULE_1,$(yvar),$(sample)))))

define DONUT_RULE_2
../output/nonparametric_rd_density_donut_log_$(1)_$(BW_LABEL)_$(2)_donut$(DONUT_2_LABEL).pdf: nonparametric_rd_density_donut.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_donut.R $(1) $(BW_M) $(2) $(FE_SPEC) $(BINS_PER_SIDE) $(DONUT_2_M)
endef
$(foreach yvar,$(YVARS),$(foreach sample,$(SAMPLES),$(eval $(call DONUT_RULE_2,$(yvar),$(sample)))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
