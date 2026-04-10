SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
MAIN_BW_FT := 250
APPENDIX_BW_FT := 500
BINS_PER_SIDE ?= 5
YVARS := density_far density_dupac

all: \
	../output/nonparametric_rd_density_linear_display_log_density_far_bw$(MAIN_BW_FT)_all.pdf \
	../output/nonparametric_rd_density_linear_display_log_density_dupac_bw$(MAIN_BW_FT)_all.pdf \
	../output/nonparametric_rd_density_linear_display_log_density_far_bw$(MAIN_BW_FT)_multifamily.pdf \
	../output/nonparametric_rd_density_linear_display_log_density_dupac_bw$(MAIN_BW_FT)_multifamily.pdf \
	../output/nonparametric_rd_density_linear_display_log_density_far_bw$(APPENDIX_BW_FT)_multifamily.pdf \
	../output/nonparametric_rd_density_linear_display_log_density_dupac_bw$(APPENDIX_BW_FT)_multifamily.pdf

define BASELINE_RULE
../output/nonparametric_rd_density_linear_display_log_$(1)_bw$(2)_$(3).pdf: nonparametric_rd_density_linear_display.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_linear_display.R $(1) $(2) $(3) $(FE_SPEC) $(BINS_PER_SIDE) $$@
endef
$(foreach yvar,$(YVARS),$(foreach spec,$(MAIN_BW_FT)_all $(MAIN_BW_FT)_multifamily $(APPENDIX_BW_FT)_multifamily,$(eval $(call BASELINE_RULE,$(yvar),$(word 1,$(subst _, ,$(spec))),$(word 2,$(subst _, ,$(spec)))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
