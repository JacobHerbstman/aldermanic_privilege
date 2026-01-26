SHELL := bash
include ../../shell_functions.make

BANDWIDTHS := 200 250 264 500 528 792 1000 1056 1320
KERNELS    := triangular
YVARS      := density_far density_dupac unitscount
LOG_STATUS := TRUE FALSE
DONUT := 0

LOG_PREFIX_TRUE  := log_
LOG_PREFIX_FALSE :=

define GEN_FILENAMES
    $(foreach yvar,$(YVARS), \
    $(foreach bw,$(BANDWIDTHS), \
    $(foreach kernel,$(KERNELS), \
    $(foreach log_stat,$(LOG_STATUS), \
        ../output/rd_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).pdf \
    ))))
endef

ALL_RD_PLOTS := $(call GEN_FILENAMES)

all: $(ALL_RD_PLOTS)

define GEN_PLOTTING_RULES
../output/rd_plot_$(LOG_PREFIX_$(log_stat))$(yvar)_bw$(bw)_$(kernel).pdf: spatial_rd_same_zone_only.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript spatial_rd_same_zone_only.R $(yvar) $(log_stat) $(bw) $(kernel) $$@
endef

$(foreach yvar,$(YVARS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach kernel,$(KERNELS), \
$(foreach log_stat,$(LOG_STATUS), \
    $(eval $(GEN_PLOTTING_RULES)) \
))))

../input/parcels_with_ward_distances.csv: ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
