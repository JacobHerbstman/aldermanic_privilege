SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
BW_FT := 500
BINS_PER_SIDE ?= 5
YVARS := density_far density_dupac

all: \
	../output/nonparametric_rd_density_linear_display_log_density_far_bw$(BW_FT)_multifamily.pdf \
	../output/nonparametric_rd_density_linear_display_log_density_dupac_bw$(BW_FT)_multifamily.pdf \
	../output/nonparametric_rd_density_linear_display_log_density_far_bw$(BW_FT)_all.pdf \
	../output/nonparametric_rd_density_linear_display_log_density_dupac_bw$(BW_FT)_all.pdf

define BASELINE_RULE
../output/nonparametric_rd_density_linear_display_log_$(1)_bw$(BW_FT)_$(2).pdf: nonparametric_rd_density_linear_display.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_linear_display.R $(1) $(BW_FT) $(2) $(FE_SPEC) $(BINS_PER_SIDE) $$@
endef
$(foreach yvar,$(YVARS),$(foreach sample,multifamily all,$(eval $(call BASELINE_RULE,$(yvar),$(sample)))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
