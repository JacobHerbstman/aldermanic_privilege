SHELL := bash
include ../../shell_functions.make

BANDWIDTHS := 500
FE_SPECS := pair_x_year pair_year
YVARS := density_far density_dupac unitscount
LOG_STATUS := TRUE

LOG_PREFIX_TRUE := log_
LOG_PREFIX_FALSE :=

define GEN_FILES
$(foreach yvar,$(YVARS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach fe,$(FE_SPECS), \
$(foreach log_stat,$(LOG_STATUS), \
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(fe).pdf \
))))
endef

ALL_OUTPUTS := $(call GEN_FILES)

all: $(ALL_OUTPUTS)

define GEN_RULE
../output/rd_fe_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(fe).pdf: spatial_rd_fe.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript spatial_rd_fe.R $(yvar) $(log_stat) $(bw) $(fe) $$@
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
