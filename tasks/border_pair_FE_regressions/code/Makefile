SHELL := bash
include ../../shell_functions.make

# Active uncertainty-spec FE table set used by slides_uncertainty.
FE_SPECS := pair_x_year pair_year_only zone_pair_x_year zone_pair_year_additive
BWS_FT := 250 500 1000
YVARS_TABLE := 'log(density_far)' 'log(density_dupac)' 'log(unitscount)'
UNITS_CAP ?= 40
OUTPUT_SUFFIX ?=

OUTPUT_FILES := $(foreach bw,$(BWS_FT),$(foreach fe,$(FE_SPECS),../output/fe_table_bw$(bw)_$(fe)$(OUTPUT_SUFFIX).tex))

all: $(OUTPUT_FILES)

define FE_template
../output/fe_table_bw$(1)_$(2)$(OUTPUT_SUFFIX).tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	# arg order: bw_ft units_cap fe_spec output_filename yvars...
	Rscript border_pair_FE_regressions.R $(1) $(UNITS_CAP) $(2) "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(foreach fe,$(FE_SPECS),$(eval $(call FE_template,$(bw),$(fe)))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
