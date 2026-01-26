SHELL := bash
include ../../shell_functions.make

# ─────────────────────────────────────────────────────────────────────────────
# Parameters
# ─────────────────────────────────────────────────────────────────────────────
# For quick iteration, set FE_SPECS := zone_x_pair_year to only run the default
FE_SPECS := zone_x_pair_year zone_pair_x_year triple pair_year_only pair_x_year
BWS_FT := 250 500 1000
YVARS_TABLE := 'log(density_far)' 'log(density_dupac)' 'log(unitscount)'

# ─────────────────────────────────────────────────────────────────────────────
# Generate Output Filenames
# ─────────────────────────────────────────────────────────────────────────────
OUTPUT_FILES := $(foreach bw,$(BWS_FT),$(foreach fe,$(FE_SPECS),../output/fe_table_bw$(bw)_$(fe).tex))

all: $(OUTPUT_FILES)

# ─────────────────────────────────────────────────────────────────────────────
# Rule Generation
# ─────────────────────────────────────────────────────────────────────────────
define FE_template
../output/fe_table_bw$(1)_$(2).tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript border_pair_FE_regressions.R --bw_ft=$(1) --fe_spec=$(2) --output="$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(foreach fe,$(FE_SPECS),$(eval $(call FE_template,$(bw),$(fe)))))

../input/parcels_with_ward_distances.csv: ../../calculate_ward_boundary_distances/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv
