SHELL := bash
include ../../shell_functions.make

POLY_ORDERS := 1 2 3
BANDWIDTHS  := 500
KERNELS     := triangular
YVARS       := density_dupac density_far
LOG_STATUS  := TRUE
DONUT       := 0

LOG_PREFIX_TRUE  := log_
LOG_PREFIX_FALSE :=

POLY_LABEL_1 := linear
POLY_LABEL_2 := quadratic
POLY_LABEL_3 := cubic
POLY_LABEL_4 := quartic

define GEN_FILENAMES
    $(foreach yvar,$(YVARS), \
    $(foreach bw,$(BANDWIDTHS), \
    $(foreach kernel,$(KERNELS), \
    $(foreach log_stat,$(LOG_STATUS), \
    $(foreach p,$(POLY_ORDERS), \
        ../output/rd_$(LOG_PREFIX_$(log_stat))$(yvar)_$(POLY_LABEL_$(p))_p$(p)_bw$(bw)_$(kernel).pdf \
    )))))
endef

ALL_OUTPUTS := $(call GEN_FILENAMES)

all: $(ALL_OUTPUTS)

define GEN_PLOTTING_RULES
../output/rd_$(LOG_PREFIX_$(log_stat))$(yvar)_$(POLY_LABEL_$(p))_p$(p)_bw$(bw)_$(kernel).pdf: spatial_rd_same_zone_only_function_robustness.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript spatial_rd_same_zone_only_function_robustness.R $(yvar) $(log_stat) $(bw) $(kernel) $(p) $$@
endef

$(foreach yvar,$(YVARS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach kernel,$(KERNELS), \
$(foreach log_stat,$(LOG_STATUS), \
$(foreach p,$(POLY_ORDERS), \
    $(eval $(GEN_PLOTTING_RULES)) \
)))))

../input/parcels_with_ward_distances.csv: ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
