SHELL := bash

BW_FT := 500
PLACEBO_SHIFTS_FT := -500 500
YVARS := density_far density_dupac

all: $(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),../output/placebo_nonparametric_rd_log_$(yvar)_bw$(BW_FT)_multifamily_baseline_fe_shift$(shift).pdf)) \
	../output/placebo_nonparametric_rd_summary_bw$(BW_FT)_multifamily_baseline_fe.csv

define PLACEBO_RULE
../output/placebo_nonparametric_rd_log_$(1)_bw$(BW_FT)_multifamily_baseline_fe_shift$(2).pdf: nonparametric_rd_density_placebo.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_placebo.R $(1) TRUE $(BW_FT) multifamily all $(2) $$@ feet
endef
$(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),$(eval $(call PLACEBO_RULE,$(yvar),$(shift)))))

../output/placebo_nonparametric_rd_summary_bw$(BW_FT)_multifamily_baseline_fe.csv: $(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),../output/placebo_nonparametric_rd_log_$(yvar)_bw$(BW_FT)_multifamily_baseline_fe_shift$(shift).pdf)) | ../output
	head -n 1 ../output/placebo_nonparametric_rd_log_density_far_bw$(BW_FT)_multifamily_baseline_fe_shift-500_meta.csv > $@
	tail -q -n +2 $(foreach yvar,$(YVARS),$(foreach shift,$(PLACEBO_SHIFTS_FT),../output/placebo_nonparametric_rd_log_$(yvar)_bw$(BW_FT)_multifamily_baseline_fe_shift$(shift)_meta.csv)) >> $@

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
