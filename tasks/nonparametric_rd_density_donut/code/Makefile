SHELL := bash

DONUTS_FT := 25 50
YVARS := density_far density_dupac

all: \
	$(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),../output/donut_nonparametric_rd_log_$(yvar)_bw500_multifamily_baseline_fe_donut$(donut).pdf)) \
	../output/donut_nonparametric_rd_summary_bw500_multifamily_baseline_fe.csv \
	$(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),../output/donut_nonparametric_rd_log_$(yvar)_bw250_all_baseline_fe_donut$(donut).pdf)) \
	../output/donut_nonparametric_rd_summary_bw250_all_baseline_fe.csv

define DONUT_RULE
../output/donut_nonparametric_rd_log_$(1)_bw$(2)_$(3)_baseline_fe_donut$(4).pdf: nonparametric_rd_density_donut.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript nonparametric_rd_density_donut.R $(1) TRUE $(2) $(3) all $(4) $$@ feet
endef
$(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),$(eval $(call DONUT_RULE,$(yvar),500,multifamily,$(donut)))))
$(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),$(eval $(call DONUT_RULE,$(yvar),250,all,$(donut)))))

../output/donut_nonparametric_rd_summary_bw500_multifamily_baseline_fe.csv: $(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),../output/donut_nonparametric_rd_log_$(yvar)_bw500_multifamily_baseline_fe_donut$(donut).pdf)) | ../output
	head -n 1 ../output/donut_nonparametric_rd_log_density_far_bw500_multifamily_baseline_fe_donut25_meta.csv > $@
	tail -q -n +2 $(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),../output/donut_nonparametric_rd_log_$(yvar)_bw500_multifamily_baseline_fe_donut$(donut)_meta.csv)) >> $@

../output/donut_nonparametric_rd_summary_bw250_all_baseline_fe.csv: $(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),../output/donut_nonparametric_rd_log_$(yvar)_bw250_all_baseline_fe_donut$(donut).pdf)) | ../output
	head -n 1 ../output/donut_nonparametric_rd_log_density_far_bw250_all_baseline_fe_donut25_meta.csv > $@
	tail -q -n +2 $(foreach yvar,$(YVARS),$(foreach donut,$(DONUTS_FT),../output/donut_nonparametric_rd_log_$(yvar)_bw250_all_baseline_fe_donut$(donut)_meta.csv)) >> $@

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
