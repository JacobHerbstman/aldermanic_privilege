SHELL := bash
include ../../shell_functions.make

BWS_FT := 100 150 200 250 264 500 528 750 792 1000 1056 1320
YVARS_TABLE := 'log(density_far)' 'log(density_dupac)' 'log(unitscount)'

OUTPUT_FILES := $(foreach bw,$(BWS_FT),../output/fe_table_bw$(bw).tex)

all: $(OUTPUT_FILES)

define FE_template
../output/fe_table_bw$(1).tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript border_pair_FE_regressions.R "$(1)" "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(eval $(call FE_template,$(bw))))

../input/parcels_with_ward_distances.csv: ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
