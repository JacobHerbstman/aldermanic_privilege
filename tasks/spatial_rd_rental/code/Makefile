SHELL := bash
include ../../shell_functions.make

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
# Sample toggle (Override with `make SAMPLE=FALSE` for full run)
SAMPLE := TRUE

ifeq ($(SAMPLE),TRUE)
  INPUT_NAME := rent_with_ward_distances_sample.parquet
  TAG := _sample
else
  INPUT_NAME := rent_with_ward_distances_full.parquet
  TAG := _full
endif

INPUT_FILE := ../input/$(INPUT_NAME)
SCRIPT_FILE := spatial_rd_rental.R

# Params
YVARS := rent_price
LOGS  := TRUE
BWS   := 50 100 200 250 500 750 1000 1320 2640
KERNELS := triangular uniform

# -----------------------------------------------------------------------------
# Targets
# -----------------------------------------------------------------------------
# Generate list of output files
# Pattern: rd_plot_log_rent_price_bw500_triangular_sample.pdf
OUTPUTS := $(foreach y,$(YVARS), \
             $(foreach l,$(LOGS), \
               $(foreach b,$(BWS), \
                 $(foreach k,$(KERNELS), \
                   ../output/rd_plot_$(if $(filter TRUE,$(l)),log_,)$(y)_bw$(b)_$(k)$(TAG).pdf))))

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUTS)

# -----------------------------------------------------------------------------
# Rules
# -----------------------------------------------------------------------------

# Rule to build the plots
# Pass INPUT_FILE as the first argument to the R script
define build-rd
../output/rd_plot_$(if $(filter TRUE,$(2)),log_,)$(1)_bw$(3)_$(4)$(TAG).pdf: $(SCRIPT_FILE) $(INPUT_FILE) | ../output
	@echo "-> Running RD for $(1) (log=$(2), bw=$(3), k=$(4)) on $(INPUT_NAME)"
	$$(R) $(SCRIPT_FILE) "$(INPUT_FILE)" "$(1)" "$(2)" "$(3)" "$(4)" "$$@"
endef

$(foreach y,$(YVARS), \
  $(foreach l,$(LOGS), \
    $(foreach b,$(BWS), \
      $(foreach k,$(KERNELS), \
        $(eval $(call build-rd,$(y),$(l),$(b),$(k)))))))

# Link Inputs
link-inputs:
	ln -sf ../../calculate_rent_distances/output/$(INPUT_NAME) ../input/

$(INPUT_FILE): | link-inputs

include ../../generic.make