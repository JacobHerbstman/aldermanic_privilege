SHELL := bash
include ../../shell_functions.make

BWS_FT := 250
DONUTS_FT := 25 50
YVARS_TABLE := 'log(density_far)' 'log(density_dupac)' 'log(unitscount)'

define GEN_OUTPUTS
$(foreach bw,$(BWS_FT),$(foreach donut,$(DONUTS_FT),../output/fe_table_bw$(bw)_donut$(donut).tex ))
endef
OUTPUT_FILES := $(call GEN_OUTPUTS)

all: $(OUTPUT_FILES)

define FE_template
../output/fe_table_bw$(1)_donut$(2).tex: border_pair_FE_regressions_donut.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript border_pair_FE_regressions_donut.R "$(1)" "$(2)" "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(foreach donut,$(DONUTS_FT),$(eval $(call FE_template,$(bw),$(donut)))))

../input/parcels_with_ward_distances.csv: ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
