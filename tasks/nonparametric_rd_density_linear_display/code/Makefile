SHELL := bash

FE_SPEC := zonegroup_segment_year_additive
MAIN_BW_M ?= 100
MAIN_BW_FT := $(shell Rscript -e 'cat(round($(MAIN_BW_M) / 0.3048))')
APPENDIX_BW_M ?= 100
APPENDIX_BW_FT := $(shell Rscript -e 'cat(round($(APPENDIX_BW_M) / 0.3048))')
BINS_PER_SIDE ?= 5
YVARS := density_far density_dupac

all: \
	../output/nonparametric_rd_density_linear_display_log_density_far_$(MAIN_BW_M)m_all_bins$(BINS_PER_SIDE).pdf \
	../output/nonparametric_rd_density_linear_display_log_density_dupac_$(MAIN_BW_M)m_all_bins$(BINS_PER_SIDE).pdf \
	../output/nonparametric_rd_density_linear_display_log_density_far_$(APPENDIX_BW_M)m_multifamily_bins$(BINS_PER_SIDE).pdf \
	../output/nonparametric_rd_density_linear_display_log_density_dupac_$(APPENDIX_BW_M)m_multifamily_bins$(BINS_PER_SIDE).pdf

define METER_RULE
../output/nonparametric_rd_density_linear_display_log_$(1)_$(2)m_$(4)_bins$(BINS_PER_SIDE).pdf: nonparametric_rd_density_linear_display.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_linear_display.R $(1) $(3) $(4) $(FE_SPEC) $(BINS_PER_SIDE) ../input/parcels_with_ward_distances.csv $$@
endef
$(foreach yvar,$(YVARS),$(foreach spec,$(MAIN_BW_M)_$(MAIN_BW_FT)_all $(APPENDIX_BW_M)_$(APPENDIX_BW_FT)_multifamily,$(eval $(call METER_RULE,$(yvar),$(word 1,$(subst _, ,$(spec))),$(word 2,$(subst _, ,$(spec))),$(word 3,$(subst _, ,$(spec)))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
