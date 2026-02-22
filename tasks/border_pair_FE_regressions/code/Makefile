SHELL := bash
include ../../shell_functions.make

# Active uncertainty-spec FE table set used by slides_uncertainty.
FE_SPECS := pair_x_year pair_year_only pair_x_year_far pair_year_far zone_pair_x_year zone_pair_year_additive
BWS_FT := 250 500 1000
SAMPLES := all multifamily
YVARS_TABLE := 'log(density_far)' 'log(density_dupac)' 'log(unitscount)'

OUTPUT_FILES := $(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(FE_SPECS),../output/fe_table_bw$(bw)_$(sample)_$(fe).tex)))

all: $(OUTPUT_FILES)

define FE_template
../output/fe_table_bw$(1)_$(2)_$(3).tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	# arg order: bw_ft sample fe_spec output_filename yvars...
	Rscript border_pair_FE_regressions.R $(1) $(2) $(3) "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(FE_SPECS),$(eval $(call FE_template,$(bw),$(sample),$(fe))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
