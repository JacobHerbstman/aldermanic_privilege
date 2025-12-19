SHELL := bash
include ../../shell_functions.make

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
# Sample toggle (Override with `make SAMPLE=FALSE` for full run)
SAMPLE := FALSE

ifeq ($(SAMPLE),TRUE)
  INPUT_NAME := rent_with_ward_distances_sample.parquet
  TAG := _sample
else
  INPUT_NAME := rent_with_ward_distances_full.parquet
  TAG := _full
endif

INPUT_FILE := ../input/$(INPUT_NAME)
SCRIPT_FILE := spatial_rd_rental_conley.R

# Params
YVARS := rent_price
LOGS  := TRUE
BWS   := 50 100 250 500
KERNELS := triangular
CONLEY_CUTOFFS := .01 .05 0.1 0.25
RESID_TYPES := none border_x_month

# NOTE: Full grid is 4 bw × 4 conley × 3 resid = 48 plots
# To run a subset, override on command line, e.g.:
#   make BWS="250" RESID_TYPES="border_x_month" CONLEY_CUTOFFS="0.5"

# -----------------------------------------------------------------------------
# Targets
# -----------------------------------------------------------------------------
# Generate list of output files
# Pattern: rd_plot_log_rent_price_bw500_triangular_conley0.5_resid-border_full.pdf
OUTPUTS := $(foreach y,$(YVARS), \
             $(foreach l,$(LOGS), \
               $(foreach b,$(BWS), \
                 $(foreach k,$(KERNELS), \
                   $(foreach c,$(CONLEY_CUTOFFS), \
                     $(foreach r,$(RESID_TYPES), \
                       ../output/rd_plot_$(if $(filter TRUE,$(l)),log_,)$(y)_bw$(b)_$(k)_conley$(c)_resid-$(r)$(TAG).pdf))))))

.DEFAULT_GOAL := all
.PHONY: all link-inputs clean

all: $(OUTPUTS)

# -----------------------------------------------------------------------------
# Rules
# -----------------------------------------------------------------------------

# Rule to build the plots
# Arguments: input_file yvar use_log bw kernel conley_cutoff resid_type output_file
define build-rd
../output/rd_plot_$(if $(filter TRUE,$(2)),log_,)$(1)_bw$(3)_$(4)_conley$(5)_resid-$(6)$(TAG).pdf: $(SCRIPT_FILE) $(INPUT_FILE) | ../output
	@echo "-> Running RD for $(1) (log=$(2), bw=$(3), k=$(4), conley=$(5)km, resid=$(6)) on $(INPUT_NAME)"
	$$(R) $(SCRIPT_FILE) "$(INPUT_FILE)" "$(1)" "$(2)" "$(3)" "$(4)" "$(5)" "$(6)" "$$@"
endef

$(foreach y,$(YVARS), \
  $(foreach l,$(LOGS), \
    $(foreach b,$(BWS), \
      $(foreach k,$(KERNELS), \
        $(foreach c,$(CONLEY_CUTOFFS), \
          $(foreach r,$(RESID_TYPES), \
            $(eval $(call build-rd,$(y),$(l),$(b),$(k),$(c),$(r)))))))))

# Link Inputs
link-inputs:
	ln -sf ../../calculate_rent_distances/output/$(INPUT_NAME) ../input/

$(INPUT_FILE): | link-inputs

include ../../generic.make
