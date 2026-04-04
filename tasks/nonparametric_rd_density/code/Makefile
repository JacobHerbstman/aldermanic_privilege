SHELL := bash

BW_FT := 250 500
SAMPLES := multifamily all
YVARS := density_far density_dupac
GAP_SPLITS := above_median below_median

all: $(foreach bw,$(BW_FT),$(foreach sample,$(SAMPLES),$(foreach yvar,$(YVARS),../output/nonparametric_rd_log_$(yvar)_bw$(bw)_$(sample)_baseline_fe.pdf)))

define FIGURE_RULE
../output/nonparametric_rd_log_$(1)_bw$(2)_$(3)_baseline_fe.pdf: nonparametric_rd_density.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density.R $(1) TRUE $(2) $(3) all $$@ feet
endef
$(foreach yvar,$(YVARS),$(foreach bw,$(BW_FT),$(foreach sample,$(SAMPLES),$(eval $(call FIGURE_RULE,$(yvar),$(bw),$(sample))))))

define GAP_RULE
../output/nonparametric_rd_log_$(1)_bw$(2)_$(3)_baseline_fe_gap_$(4).pdf: nonparametric_rd_density.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density.R $(1) TRUE $(2) $(3) $(4) $$@ feet
endef
$(foreach yvar,$(YVARS),$(foreach bw,$(BW_FT),$(foreach sample,$(SAMPLES),$(foreach gap,$(GAP_SPLITS),$(eval $(call GAP_RULE,$(yvar),$(bw),$(sample),$(gap)))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
