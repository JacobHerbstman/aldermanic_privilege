SHELL := bash
include ../../shell_functions.make

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
INPUT_FILE := ../input/rent_with_ward_distances.parquet
SCRIPT_FILE := spatial_rd_rental.R

# Params
YVARS := rent_price
LOGS  := TRUE
BWS   := 50 100 200 250 500 750 1000 1320 2640
KERNELS := triangular

# -----------------------------------------------------------------------------
# Targets
# -----------------------------------------------------------------------------
# Generate list of output files
# Pattern: rd_plot_log_rent_price_bw500_triangular.pdf
OUTPUTS := $(foreach y,$(YVARS), \
             $(foreach l,$(LOGS), \
               $(foreach b,$(BWS), \
                 $(foreach k,$(KERNELS), \
                   ../output/rd_plot_$(if $(filter TRUE,$(l)),log_,)$(y)_bw$(b)_$(k).pdf))))

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(OUTPUTS)

# -----------------------------------------------------------------------------
# Rules
# -----------------------------------------------------------------------------

# Rule to build the plots
# Note: We use a static pattern rule or just a foreach eval for flexibility
define build-rd
../output/rd_plot_$(if $(filter TRUE,$(2)),log_,)$(1)_bw$(3)_$(4).pdf: $(SCRIPT_FILE) $(INPUT_FILE) | ../output
	@echo "-> Running RD for $(1) (log=$(2), bw=$(3), k=$(4))"
	$$(R) $(SCRIPT_FILE) "$(1)" "$(2)" "$(3)" "$(4)" "$$@"
endef

$(foreach y,$(YVARS), \
  $(foreach l,$(LOGS), \
    $(foreach b,$(BWS), \
      $(foreach k,$(KERNELS), \
        $(eval $(call build-rd,$(y),$(l),$(b),$(k)))))))

# Link Inputs
link-inputs:
	ln -sf ../../calculate_rent_distances/output/rent_with_ward_distances.parquet ../input/

$(INPUT_FILE): | link-inputs

include ../../generic.make