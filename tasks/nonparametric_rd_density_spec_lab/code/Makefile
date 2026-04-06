SHELL := bash

BW_FT := 250 500
YVARS := density_far density_dupac
FE_SPECS := segment_zonegroup_year segment_year wardpair_zonegroup_year
FIGURE_TYPES := type3_local_linear

ALL_PDFS := $(foreach figure,$(FIGURE_TYPES), \
$(foreach fe,$(FE_SPECS), \
$(foreach bw,$(BW_FT), \
$(foreach yvar,$(YVARS),../output/$(figure)_log_$(yvar)_bw$(bw)_all_$(fe).pdf))))

MULTIFAMILY_PDFS := $(foreach figure,$(FIGURE_TYPES), \
$(foreach fe,$(FE_SPECS), \
$(foreach yvar,$(YVARS),../output/$(figure)_log_$(yvar)_bw500_multifamily_$(fe).pdf)))

PDFS := $(ALL_PDFS) $(MULTIFAMILY_PDFS)

all: $(PDFS)

define GRID_RULE
../output/$(1)_log_$(2)_bw$(3)_$(4)_$(5).pdf: nonparametric_rd_density_spec_lab.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_spec_lab.R $(1) $(2) $(3) $(4) $(5) $$@ none
endef
$(foreach figure,$(FIGURE_TYPES), \
$(foreach fe,$(FE_SPECS), \
$(foreach bw,$(BW_FT), \
$(foreach yvar,$(YVARS), \
$(eval $(call GRID_RULE,$(figure),$(yvar),$(bw),all,$(fe)))))))

$(foreach figure,$(FIGURE_TYPES), \
$(foreach fe,$(FE_SPECS), \
$(foreach yvar,$(YVARS), \
$(eval $(call GRID_RULE,$(figure),$(yvar),500,multifamily,$(fe))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
