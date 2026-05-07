SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
BW_M ?= 100
BW_FT := $(shell Rscript -e 'cat(round($(BW_M) / 0.3048))')
BINS_PER_SIDE ?= 5
SAMPLE ?= all
DONUT_1_M ?= 5
DONUT_1_FT := $(shell Rscript -e 'cat(round($(DONUT_1_M) / 0.3048))')
DONUT_2_M ?= 10
DONUT_2_FT := $(shell Rscript -e 'cat(round($(DONUT_2_M) / 0.3048))')
YVARS := density_far density_dupac

all: $(foreach yvar,$(YVARS),../output/nonparametric_rd_density_donut_log_$(yvar)_$(BW_M)m_$(SAMPLE)_donut$(DONUT_1_M)m.pdf ../output/nonparametric_rd_density_donut_log_$(yvar)_$(BW_M)m_$(SAMPLE)_donut$(DONUT_2_M)m.pdf)

define DONUT_RULE_1
../output/nonparametric_rd_density_donut_log_$(1)_$(BW_M)m_$(SAMPLE)_donut$(DONUT_1_M)m.pdf: nonparametric_rd_density_donut.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_donut.R $(1) $(BW_FT) $(SAMPLE) $(FE_SPEC) $(BINS_PER_SIDE) $(DONUT_1_FT) ../input/parcels_with_ward_distances.csv $$@
endef
$(foreach yvar,$(YVARS),$(eval $(call DONUT_RULE_1,$(yvar))))

define DONUT_RULE_2
../output/nonparametric_rd_density_donut_log_$(1)_$(BW_M)m_$(SAMPLE)_donut$(DONUT_2_M)m.pdf: nonparametric_rd_density_donut.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_donut.R $(1) $(BW_FT) $(SAMPLE) $(FE_SPEC) $(BINS_PER_SIDE) $(DONUT_2_FT) ../input/parcels_with_ward_distances.csv $$@
endef
$(foreach yvar,$(YVARS),$(eval $(call DONUT_RULE_2,$(yvar))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
