SHELL := bash
include ../../shell_functions.make

PLACEBO_SHIFTS := -500 -250 250 500
BANDWIDTHS     := 500
KERNELS        := triangular
YVARS          := density_dupac density_far
LOG_STATUS     := TRUE

LOG_PREFIX_TRUE  := log_
LOG_PREFIX_FALSE :=

define shift_to_filename
$(if $(filter -%,$(1)),minus$(subst -,,$(1)),plus$(1))
endef

define GEN_FILENAMES
    $(foreach yvar,$(YVARS), \
    $(foreach bw,$(BANDWIDTHS), \
    $(foreach kernel,$(KERNELS), \
    $(foreach log_stat,$(LOG_STATUS), \
    $(foreach s,$(PLACEBO_SHIFTS), \
        ../output/placebo_rd_$(LOG_PREFIX_$(log_stat))$(yvar)_shift_$(call shift_to_filename,$(s))_bw$(bw)_$(kernel).pdf \
    )))))
endef

ALL_OUTPUTS := $(call GEN_FILENAMES)

all: $(ALL_OUTPUTS)

define GEN_PLOTTING_RULES
../output/placebo_rd_$(LOG_PREFIX_$(log_stat))$(yvar)_shift_$(call shift_to_filename,$(s))_bw$(bw)_$(kernel).pdf: spatial_rd_same_zone_only_placebo_boundaries.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript spatial_rd_same_zone_only_placebo_boundaries.R $(yvar) $(log_stat) $(bw) $(kernel) $(s) $$@
endef

$(foreach yvar,$(YVARS), \
$(foreach bw,$(BANDWIDTHS), \
$(foreach kernel,$(KERNELS), \
$(foreach log_stat,$(LOG_STATUS), \
$(foreach s,$(PLACEBO_SHIFTS), \
    $(eval $(GEN_PLOTTING_RULES)) \
)))))

../input/parcels_with_ward_distances.csv: ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
