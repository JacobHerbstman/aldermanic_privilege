SHELL := bash

BW_FT := 250 500
SAMPLES := multifamily all
YVARS := density_far density_dupac

all: $(foreach bw,$(BW_FT),$(foreach sample,$(SAMPLES),$(foreach yvar,$(YVARS),../output/nonparametric_rd_linear_display_log_$(yvar)_bw$(bw)_$(sample)_baseline_fe.pdf)))

define FIGURE_RULE
../output/nonparametric_rd_linear_display_log_$(1)_bw$(2)_$(3)_baseline_fe.pdf: ../../nonparametric_rd_density/code/nonparametric_rd_density.R ../input/parcels_with_ward_distances.csv | ../output
	RD_PLOT_STYLE=linear_display RD_DISPLAY_ESTIMATE_SOURCE=linear_cutoff Rscript ../../nonparametric_rd_density/code/nonparametric_rd_density.R $(1) TRUE $(2) $(3) all $$@ feet
endef
$(foreach yvar,$(YVARS),$(foreach bw,$(BW_FT),$(foreach sample,$(SAMPLES),$(eval $(call FIGURE_RULE,$(yvar),$(bw),$(sample))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
