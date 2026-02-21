SHELL := bash
include ../../shell_functions.make

BANDWIDTHS := 500
FE_SPECS := pair_x_year pair_year zone_pair_year_additive
YVARS := density_far density_dupac unitscount
LOG_STATUS := TRUE FALSE

LOG_PREFIX_TRUE := log_
LOG_PREFIX_FALSE :=

SLOPE_OUTPUTS := \
../output/rd_fe_plot_log_density_far_bw500_pair_x_year.pdf \
../output/rd_fe_plot_log_density_dupac_bw500_pair_x_year.pdf \
../output/rd_fe_plot_density_far_bw500_pair_x_year.pdf \
../output/rd_fe_plot_density_dupac_bw500_pair_x_year.pdf \
../output/rd_fe_plot_log_unitscount_bw500_pair_x_year.pdf \
../output/rd_fe_plot_unitscount_bw500_pair_x_year.pdf \
../output/rd_fe_plot_log_density_far_bw500_pair_year.pdf \
../output/rd_fe_plot_log_density_dupac_bw500_pair_year.pdf \
../output/rd_fe_plot_log_density_far_bw500_zone_pair_year_additive.pdf \
../output/rd_fe_plot_log_density_dupac_bw500_zone_pair_year_additive.pdf

ALL_OUTPUTS := $(SLOPE_OUTPUTS)

all: $(ALL_OUTPUTS)

define GEN_RULE
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(fe).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript spatial_rd_fe.R $(yvar) $(log_stat) $(bw) $(fe) $$@ slope
	rm -f "$$(@:.pdf=_bins.csv)" "$$(@:.pdf=_meta.csv)"
endef

$(foreach yvar,$(YVARS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach fe,$(FE_SPECS), \
$(foreach log_stat,$(LOG_STATUS), \
$(eval $(GEN_RULE)) \
))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
