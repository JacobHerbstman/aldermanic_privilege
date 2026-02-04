SHELL := bash
include ../../shell_functions.make

BWS := 1000
BIN_MODES := kulka_fixed
FAR_LOG_STATUS := TRUE
DUPAC_LOG_STATUS := TRUE
FE_SPECS ?= pair_x_year pair_year

LOG_PREFIX_TRUE := log_
LOG_PREFIX_FALSE :=

define GEN_FAR_FIGS
$(foreach bw,$(BWS), \
$(foreach mode,$(BIN_MODES), \
$(foreach log_stat,$(FAR_LOG_STATUS), \
$(foreach fe,$(FE_SPECS), \
../output/nonparametric_rd_$(LOG_PREFIX_$(log_stat))density_far_bw$(bw)_$(mode)_$(fe).pdf \
))))
endef

define GEN_DUPAC_FIGS
$(foreach bw,$(BWS), \
$(foreach mode,$(BIN_MODES), \
$(foreach fe,$(FE_SPECS), \
../output/nonparametric_rd_log_density_dupac_bw$(bw)_$(mode)_$(fe).pdf \
)))
endef

ALL_FIGS := $(call GEN_FAR_FIGS) $(call GEN_DUPAC_FIGS)
SUMMARY_OUT := ../output/nonparametric_spec_comparison.csv ../output/nonparametric_spec_selection.csv ../output/nonparametric_spec_memo.md
SUMMARY_STAMP := ../output/nonparametric_spec_summary.stamp

all: $(ALL_FIGS) $(SUMMARY_OUT)

define GEN_FAR_RULE
../output/nonparametric_rd_$(LOG_PREFIX_$(log_stat))density_far_bw$(bw)_$(mode)_$(fe).pdf: nonparametric_rd_focused.R ../input/parcels_with_ward_distances.csv | ../output
	OUT="$$@"; \
	Rscript nonparametric_rd_focused.R \
		--yvar=density_far \
		--use_log=$(log_stat) \
		--bw_ft=$(bw) \
		--bin_mode=$(mode) \
		--bin_ft=105.6 \
		--fe_spec=$(fe) \
		--output_pdf="$$$$OUT" \
		--output_csv="$$$${OUT%.pdf}_bins.csv"
endef

define GEN_DUPAC_RULE
../output/nonparametric_rd_log_density_dupac_bw$(bw)_$(mode)_$(fe).pdf: nonparametric_rd_focused.R ../input/parcels_with_ward_distances.csv | ../output
	OUT="$$@"; \
	Rscript nonparametric_rd_focused.R \
		--yvar=density_dupac \
		--use_log=TRUE \
		--bw_ft=$(bw) \
		--bin_mode=$(mode) \
		--bin_ft=105.6 \
		--fe_spec=$(fe) \
		--output_pdf="$$$$OUT" \
		--output_csv="$$$${OUT%.pdf}_bins.csv"
endef

$(foreach bw,$(BWS), \
$(foreach mode,$(BIN_MODES), \
$(foreach log_stat,$(FAR_LOG_STATUS), \
$(foreach fe,$(FE_SPECS), \
$(eval $(GEN_FAR_RULE))))))

$(foreach bw,$(BWS), \
$(foreach mode,$(BIN_MODES), \
$(foreach fe,$(FE_SPECS), \
$(eval $(GEN_DUPAC_RULE)))))

$(SUMMARY_STAMP): compare_np_specs.R $(ALL_FIGS) | ../output
	Rscript compare_np_specs.R
	touch $@

$(SUMMARY_OUT): $(SUMMARY_STAMP)

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
